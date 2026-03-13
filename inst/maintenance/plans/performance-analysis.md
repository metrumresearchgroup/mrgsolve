# Performance Analysis: devtran.cpp, datarecord.cpp, dataobject.cpp

Scenario: Individual with many observation records (e.g., time 0 to 672 hours,
one observation every hour) takes longer than expected.

---

## 1. `reclist` is `deque<shared_ptr<datarecord>>` — biggest structural problem

Defined at `inst/include/datarecord.h:32`:
```cpp
typedef std::shared_ptr<datarecord> rec_ptr;
typedef std::deque<rec_ptr> reclist;
```

Each `datarecord` is small (~100 bytes of plain data), but every record is
individually heap-allocated via `make_shared`. For 673 observation records per
individual, that's 673 separate heap allocations, each with `shared_ptr` control
block overhead (~32 bytes of atomic reference counters).

The cost compounds during **sorting** — every swap of two `shared_ptr`s involves
**atomic increment/decrement operations** on the reference counters. `std::sort`
does O(n log n) swaps, so for n=673 you're looking at ~6,000+ atomic operations
per sort. Atomics prevent CPU instruction reordering and can cause cache line
contention.

The `deque` itself stores pointers in non-contiguous blocks, so iterating through
records in the main simulation loop (`src/devtran.cpp:458`) involves
pointer-chasing across memory blocks — poor cache locality for a hot loop that
runs for every record.

## 2. Sorting the entire tail inside the per-record simulation loop

At `src/devtran.cpp:638-640`:
```cpp
if(sort_recs) {
    std::sort(a[i].begin()+j+1, a[i].end(), CompRec());
}
```

`sort_recs` is true when `needs_sorting()` returns true
(`inst/include/datarecord.h:110`: `Addl > 0 || Ss == 1`). If a dose with `addl`
appears early in the record list for an individual with 673 observation records,
this sorts the **entire remaining tail** — 600+ `shared_ptr` elements, each
comparison dereferencing two pointers to scattered heap locations.

The `schedule()` method (`src/datarecord.cpp:537-589`) pushes all additional dose
records via `push_back` (unsorted), then the caller does a full sort. If there's
a lag time, `schedule` creates **2x addl** records (line 549). So a dose with
`addl=24` and lag time generates 48 new records, all appended to the tail,
followed by a sort of the entire tail.

The sort could be replaced by a **merge** — the existing records are already
sorted, and the new `addl` records are generated in time order. A merge is O(n)
vs O(n log n).

## 3. `insert_record` is O(n) — linear scan + deque mid-insert

At `src/datarecord.cpp:618-636`:
```cpp
void insert_record(reclist& thisi, const size_t start, rec_ptr& rec,
                   const bool put_ev_first) {
    // ... linear scan to find position ...
    thisi.insert(thisi.begin() + i, rec);
}
```

This does a linear scan from `start` to find the insertion point, then
`deque::insert` in the middle shifts all subsequent elements — O(n) for a deque.
With 673+ records, every lag-time record or infusion-off insertion costs O(673).

This is called from the main loop for:
- Lagged dose records (`src/devtran.cpp:608`)
- Infusion-off records (`src/devtran.cpp:634`)
- Modeled event records (`src/devtran.cpp:764`)

## 4. `steady_infusion` sorts the ENTIRE reclist

At `src/datarecord.cpp:463`:
```cpp
std::sort(thisi.begin(), thisi.end(), CompRec());
```

When an SS infusion record is processed, infusion-off events are pushed onto the
back of the reclist, then the **entire list** is sorted — all 673+ observation
records included. This is a full O(n log n) sort of `shared_ptr`s with all the
atomic/cache overhead.

## 5. `insert_observations` re-sorts the tail on every call

At `src/datarecord.cpp:605`:
```cpp
std::sort(thisi.begin()+start+1, thisi.end(), CompRec());
```

If modeled observations are used (mtime), each call to `insert_observations`
appends records then sorts the entire remaining tail. If `any_mtime()` triggers
at multiple records, this compounds.

## 6. `advance()` doesn't short-circuit when `tfrom == tto`

At `src/odeproblem.cpp:391-416`: when multiple records share the same time,
`advance` is called with `tfrom == tto`. For Advan 13 (LSODA), this enters the
solver machinery unnecessarily. For Advan 2/4, the analytical solution computes
`dt=0` and runs through `PolyExp` calculations that produce trivial results. A
simple `if(tto == tfrom) return;` guard would avoid this.

## 7. `init_call_record` is called for every observation record

At `src/devtran.cpp:536`:
```cpp
if(!this_rec->is_lagged()) {
    prob->init_call_record(tto);
}
```

This calls the model's `$MAIN` function through a function pointer for **every**
record, including pure observation records where parameters haven't changed. With
673 observations, that's 673 calls to `$MAIN`. If the model's `$MAIN` does
non-trivial work (e.g., covariate-dependent parameter calculations), this adds
up. In NONMEM, `$PK` is similarly called for every record, so this may be
intentional — but it's worth noting as a potential optimization target for
observation-only records when covariates haven't changed.

---

## Summary

The core issue is that data structure choices (`shared_ptr` + `deque`) impose
significant constant-factor overhead that becomes dominant when the number of
records per individual is large. The sorting patterns compound this — several
O(n log n) sorts of `shared_ptr` containers happen inside per-record or per-event
loops. For 673 observations, the algorithmic complexity of the "bookkeeping"
(sorting, inserting) can rival or exceed the actual simulation work.

### Highest-impact changes (keeping deque)

The switch from `vector` to `deque` in PR #1186 yielded 16-32x speedups for
insertion-heavy scenarios (addl with lag/infusion), so reverting to `vector` is
not advisable. The problem is scenario-dependent:

- **Insertion-heavy** (addl, lag, infusion): deque's mid-insert beats vector's
  mid-insert + reallocation
- **Observation-heavy** (many tgrid/obs records): the repeated `std::sort` calls
  on the deque are the bottleneck

The key insight is that every time records are added, both the existing records
and the new records are **already sorted** — we just need to merge them, not
re-sort from scratch.

1. **`schedule()`** — addl records are generated in time order. Instead of
   `push_back` then `std::sort`, use `std::inplace_merge` after appending:
   ```cpp
   auto mid = thisi.end(); // mark boundary before appending
   // ... push_back the new records ...
   std::inplace_merge(thisi.begin()+start, mid, thisi.end(), CompRec());
   ```
   This is O(n) vs O(n log n).

2. **`steady_infusion` / `insert_observations`** — same pattern: append sorted
   records, then `inplace_merge` instead of full `sort`.

3. **Single-record insertions** (lag, infusion-off) — the current
   `insert_record` does a linear scan. Replace with `std::lower_bound` for
   O(log n) binary search to find the insertion position. The `deque::insert` at
   that position is still O(n) element shifts, but you save the scan time and —
   critically — you avoid the separate sort call entirely.

4. **`advance()` short-circuit** — `if(tto == tfrom) return;` at the top.

5. **`shared_ptr` → `unique_ptr`** — records aren't shared across owners. This
   eliminates atomic ref-count operations on every sort comparison/swap, which is
   pure overhead on the hot path. This is the change with the best
   effort-to-impact ratio.

---

## Detailed example: `schedule()` with `inplace_merge`

### Current flow

`schedule()` (datarecord.cpp:537-589) pushes new addl records via `push_back` in
time order. Then back in the caller (devtran.cpp:638-640), the entire tail of the
record list is sorted:

```cpp
if(sort_recs) {
    std::sort(a[i].begin()+j+1, a[i].end(), CompRec());
}
```

For an individual with 600 observation records remaining after position `j`, plus
24 new addl records, this sorts all 624 elements — O(624 * log(624)) ≈ 5,600
comparisons, each dereferencing two `shared_ptr`s.

### The insight

Both halves are already sorted:
- `[j+1, merge_point)` — the existing observation/dose records, already in order
- `[merge_point, end)` — the new addl records, generated in increasing time order
  (the loop increments `k`, so `ontime = Time + Ii*k` is monotonically
  increasing; parent records at `parent_time + Ii*k` are earlier than their
  corresponding dose records, so each parent-dose pair is also in order)

`std::inplace_merge(first, middle, last, comp)` takes two contiguous sorted
ranges `[first, middle)` and `[middle, last)` and combines them into one sorted
range `[first, last)`. It's O(n) with extra memory (which the standard library
allocates automatically), vs O(n log n) for a full sort.

### What the change looks like in devtran.cpp

Replace the sort-after-schedule block (around lines 609-640):

```cpp
// Before scheduling, mark where new records will start
auto merge_point = a[i].end();

// Schedule pushes sorted addl records onto the back
if(has_lagt) {
    // ... create lagged record, insert_record ...
    newev->schedule(a[i], maxtime, put_ev_first, NN, prob.alag(this_cmtn));
    sort_recs = newev->needs_sorting();
} else {
    this_rec->schedule(a[i], maxtime, addl_ev_first, NN, 0.0);
    sort_recs = this_rec->needs_sorting();
}

// ... infusion-off insertion ...

// MERGE instead of SORT
if(sort_recs) {
    std::inplace_merge(a[i].begin()+j+1, merge_point, a[i].end(), CompRec());
}
```

### Before vs after for 600 obs + 24 addl records

| Operation          | `std::sort`     | `std::inplace_merge` |
|--------------------|-----------------|----------------------|
| Comparisons        | ~5,600          | ~624                 |
| shared_ptr derefs  | ~11,200         | ~1,248               |
| Complexity         | O(n log n)      | O(n)                 |

The merge does a single linear pass, interleaving the two sorted runs. It walks
through both sequences with two "cursors," picking the smaller element each time
— essentially the merge step from merge sort, applied to two already-sorted
halves.

---

## Detailed example: `steady_infusion` with `inplace_merge`

### Current code (datarecord.cpp:451-463)

```cpp
// Add on infusion off events
int ninf_ss = floor(duration/this->ii());

double first_off = Time + duration - double(ninf_ss)*Ii - lagt;
if(first_off == Time) {
    first_off = duration - Ii + Time + lagt;
    --ninf_ss;
}
for(size_t k = 0; k < offs.size(); ++k) {
    offs.at(k)->time(first_off + double(k)*double(Ii));
    thisi.push_back(offs.at(k));
}
std::sort(thisi.begin(), thisi.end(), CompRec());
```

The `offs` vector contains infusion-off records created during the SS advance
loop (line 368). Their times are set in increasing order (`first_off + k*Ii`),
so the pushed sequence is already sorted. Meanwhile the existing records in
`thisi` are already sorted. Full sort of the entire reclist is unnecessary.

### Replacement

```cpp
// Add on infusion off events
int ninf_ss = floor(duration/this->ii());

double first_off = Time + duration - double(ninf_ss)*Ii - lagt;
if(first_off == Time) {
    first_off = duration - Ii + Time + lagt;
    --ninf_ss;
}

// Mark boundary before appending
auto merge_point = thisi.end();

for(size_t k = 0; k < offs.size(); ++k) {
    offs.at(k)->time(first_off + double(k)*double(Ii));
    thisi.push_back(offs.at(k));
}

// Merge two sorted runs instead of sorting everything
std::inplace_merge(thisi.begin(), merge_point, thisi.end(), CompRec());
```

Same idea: the existing records `[begin, merge_point)` are sorted, the new
infusion-off records `[merge_point, end)` are sorted, and `inplace_merge`
combines them in O(n). For an individual with 673 observation records and 2-3
infusion-off events, the current `std::sort` does ~6,000 comparisons; the merge
does ~676.

---

## Detailed example: `insert_observations` with `inplace_merge`

### Current code (datarecord.cpp:595-606)

```cpp
void insert_observations(reclist& thisi, mrgsolve::evdata& ev,
                         const size_t start, const bool put_ev_first) {
    const int total = ev.addl + 1;
    int nextpos = put_ev_first && (ev.evid != 1 && ev.evid != 4)
                  ? -1000000000 : 1000000000;
    for(int i = 0; i < total; ++i) {
        rec_ptr rec = NEWREC(ev.time + i*ev.ii, nextpos, false);
        rec->evid(ev.evid);
        rec->Cmt = ev.cmt;
        thisi.push_back(rec);
    }
    std::sort(thisi.begin()+start+1, thisi.end(), CompRec());
}
```

The new observation records are generated with times `ev.time + i*ev.ii` —
monotonically increasing. The existing tail `[start+1, old end)` is already
sorted. Same pattern.

### Replacement

```cpp
void insert_observations(reclist& thisi, mrgsolve::evdata& ev,
                         const size_t start, const bool put_ev_first) {
    const int total = ev.addl + 1;
    int nextpos = put_ev_first && (ev.evid != 1 && ev.evid != 4)
                  ? -1000000000 : 1000000000;

    // Mark boundary before appending
    auto merge_point = thisi.end();

    for(int i = 0; i < total; ++i) {
        rec_ptr rec = NEWREC(ev.time + i*ev.ii, nextpos, false);
        rec->evid(ev.evid);
        rec->Cmt = ev.cmt;
        thisi.push_back(rec);
    }

    // Merge two sorted runs instead of sorting the tail
    std::inplace_merge(thisi.begin()+start+1, merge_point, thisi.end(),
                       CompRec());
}
```

---

## Detailed example: `insert_record` with `lower_bound`

### Current code (datarecord.cpp:618-636)

```cpp
void insert_record(reclist& thisi, const size_t start, rec_ptr& rec,
                   const bool put_ev_first) {
    double time = rec->time();
    size_t i = start;
    if(put_ev_first) {
        for(i = start + 1; i < thisi.size(); ++i) {
            if(thisi[i]->time() >= time) {
                break;
            }
        }
    } else {
        for(i = start + 1; i < thisi.size(); ++i) {
            if(thisi[i]->time() > time) {
                break;
            }
        }
    }
    thisi.insert(thisi.begin() + i, rec);
}
```

The linear scan walks from `start+1` to find the right position — O(n). Since
the records are sorted by time, we can use binary search to find the insertion
point in O(log n). The `deque::insert` itself is still O(n) for shifting
elements, but we eliminate the linear scan AND — critically — this function
already inserts in sorted position, so the caller doesn't need a separate sort
call afterward.

### Replacement

```cpp
void insert_record(reclist& thisi, const size_t start, rec_ptr& rec,
                   const bool put_ev_first) {

    auto begin = thisi.begin() + start + 1;
    auto end = thisi.end();

    // Binary search for insertion point in sorted range
    // lower_bound: insert before first record with time >= rec's time
    //              (event goes first among ties)
    // upper_bound: insert after last record with time <= rec's time
    //              (event goes last among ties)
    auto pos = put_ev_first
        ? std::lower_bound(begin, end, rec, CompRec())
        : std::upper_bound(begin, end, rec, CompRec());

    thisi.insert(pos, rec);
}
```

Note: this requires that `CompRec` provides a strict weak ordering that's
consistent with how `lower_bound`/`upper_bound` should partition the records.
The existing `CompRec` compares by time with position-based tiebreaking, so:
- `lower_bound` with `CompRec` finds the first element that doesn't compare less
  than `rec` — i.e., the first record at or after `rec`'s time, placing the new
  record before ties (event first)
- `upper_bound` with `CompRec` finds the first element that compares greater than
  `rec` — i.e., the first record strictly after `rec`'s time, placing the new
  record after ties (event last)

The `put_ev_first` logic maps to `lower_bound` vs `upper_bound` because the
`pos` field on inserted records is set to extreme values (-1000000000 or
1000000000) that control tiebreaking within `CompRec`. You'd want to verify the
`CompRec` ordering handles the `pos` values correctly with the binary search —
but the principle is sound.

For 673 records, the scan drops from ~673 comparisons to ~10 (`log2(673)`).

---

## Feasibility: `shared_ptr` to `unique_ptr`

### One true blocker: `mtimehx` dual container storage

At devtran.cpp:764-765:
```cpp
insert_record(a[i], j, new_ev, put_ev_first);  // stored in a[i]
mtimehx.push_back(new_ev);                      // ALSO stored in mtimehx
```

The same `new_ev` lives in both `a[i]` (the main reclist) and `mtimehx` (a
deduplication history) simultaneously. With `unique_ptr`, you can't have two
owners.

**Fix:** `mtimehx` is only used for uniqueness checking via `CompEqual`
(datarecord.cpp:129-138), which just reads `time()`, `evid()`, `cmt()`, and
`amt()`. It doesn't need the actual pointer — just those four values:

```cpp
struct mtime_key { double time; unsigned int evid; int cmt; double amt; };
std::vector<mtime_key> mtimehx;
// ...
mtimehx.push_back({this_time, this_evid, this_cmt, this_amt});
```

This eliminates the shared ownership entirely and is actually cheaper than
storing `shared_ptr`s in the history list.

### 5 functions taking `rec_ptr` by value (easy fix)

These all copy the `shared_ptr` (bumping the atomic ref count) but never retain
it — they just read fields off the record:

- `set_d(rec_ptr this_rec)` — odeproblem.cpp:217
- `rate_main(rec_ptr rec, int cmtn)` — odeproblem.cpp:281
- `check_data_rate(rec_ptr rec, int cmtn)` — odeproblem.cpp:291
- `check_modeled_rate(rec_ptr rec)` — odeproblem.cpp:312
- `check_modeled_dur(rec_ptr rec)` — odeproblem.cpp:326

All change to `const rec_ptr&` (or even `const datarecord&` / `datarecord*`).
Note that `set_d` is called on the hot path for every record — that's an
unnecessary atomic increment/decrement on every iteration right now, even before
the `unique_ptr` swap.

### `offs` in `steady_infusion` — transfer, not shared ownership

Records are created in `offs`, then later pushed into `thisi`
(datarecord.cpp:459-461). This looks like shared ownership but it's actually a
transfer — change to `std::move`:

```cpp
thisi.push_back(std::move(offs.at(k)));
```

### No other blockers

- No `weak_ptr`, `use_count()`, or `shared_from_this` anywhere in the codebase
- `NEWREC(*this_rec)` at devtran.cpp:600 creates a new record via copy
  constructor — not shared ownership
- All sorting/comparison uses `const rec_ptr&`
- `insert_record` takes `rec_ptr&` by reference

### Summary of changes needed

| What | Change | Effort |
|------|--------|--------|
| `rec_ptr` typedef | `shared_ptr` → `unique_ptr` | trivial |
| `NEWREC` macro | `make_shared` → `make_unique` | trivial |
| `mtimehx` | Store value struct instead of `rec_ptr` | small refactor |
| 5 odeproblem functions | By-value → `const rec_ptr&` | trivial |
| `offs` transfers | Add `std::move` | trivial |
| Container push_back/insert | Add `std::move` where needed | mechanical |

**Bottom line:** the swap is feasible. The only real refactoring is replacing
`mtimehx` with a value-based history (which is a net improvement anyway).

---

## Detailed example: converting `mtimehx` to O(1) lookup

### Current implementation

`mtimehx` is a `reclist` (i.e., `deque<shared_ptr<datarecord>>`) used to track
which modeled events have already been inserted, preventing duplicates. It lives
at devtran.cpp:391 and is used in three places:

1. **Declaration** (line 391): `reclist mtimehx;`
2. **Uniqueness check** (line 769): `CompEqual(mtimehx, this_time, this_evid, this_cmt, this_amt)` — linear scan of the entire list
3. **Recording** (line 775): `mtimehx.push_back(new_ev)` — stores a `shared_ptr` copy
4. **Cleanup** (line 412): `mtimehx.clear()` — per-individual reset
5. **Size check** (line 792): `used_mtimehx = mtimehx.size() > 0`

`CompEqual` (datarecord.cpp:129-138) does a linear scan comparing `time()`,
`evid()`, `cmt()`, and `amt()` against every stored record. If an individual
generates many modeled events (e.g., from `$TABLE` mtime calls at every record),
this becomes O(n^2) over the simulation — each new event scans all previously
stored events.

### Problem

- **O(n) per lookup** — `CompEqual` walks the full `mtimehx` list
- **Stores full `shared_ptr`** — only 4 scalar fields are actually read
- **Blocks `unique_ptr` migration** — dual ownership of `new_ev` in both `a[i]`
  and `mtimehx`

### Step 1: Define a lightweight key struct

Add to `datarecord.h` (or a new `mrgsolve_types.h` header):

```cpp
namespace mrgsolve {

struct mtime_key {
  double time;
  unsigned int evid;
  int cmt;
  double amt;

  bool operator==(const mtime_key& other) const {
    return time == other.time &&
           evid == other.evid &&
           cmt  == other.cmt  &&
           amt  == other.amt;
  }
};

struct mtime_key_hash {
  size_t operator()(const mtime_key& k) const {
    // Combine hashes of the four fields
    size_t h = std::hash<double>{}(k.time);
    h ^= std::hash<unsigned int>{}(k.evid) + 0x9e3779b9 + (h << 6) + (h >> 2);
    h ^= std::hash<int>{}(k.cmt) + 0x9e3779b9 + (h << 6) + (h >> 2);
    h ^= std::hash<double>{}(k.amt) + 0x9e3779b9 + (h << 6) + (h >> 2);
    return h;
  }
};

} // namespace mrgsolve
```

The hash combiner uses the boost-style formula (`0x9e3779b9` is the golden
ratio constant) to mix bits from each field. This avoids collisions when fields
have similar values.

### Step 2: Replace `mtimehx` declaration in devtran.cpp

```cpp
// Before (line 391-392):
reclist mtimehx;
bool used_mtimehx = false;

// After:
std::unordered_set<mrgsolve::mtime_key, mrgsolve::mtime_key_hash> mtimehx;
bool used_mtimehx = false;
```

Add `#include <unordered_set>` at the top of devtran.cpp if not already present.

### Step 3: Replace `CompEqual` call with set lookup

```cpp
// Before (lines 769-771):
bool found = CompEqual(mtimehx, this_time, this_evid, this_cmt, this_amt);
do_mt_ev = do_mt_ev && !found;

// After:
mrgsolve::mtime_key key{this_time, this_evid, this_cmt, this_amt};
do_mt_ev = do_mt_ev && (mtimehx.find(key) == mtimehx.end());
```

### Step 4: Replace `push_back` with set insert

```cpp
// Before (line 775):
mtimehx.push_back(new_ev);

// After:
mtimehx.insert({this_time, this_evid, this_cmt, this_amt});
```

Note: `this_time`, `this_evid`, `this_cmt`, and `this_amt` are already
unpacked as local variables at devtran.cpp:685-687 and nearby, so no extra
field access is needed.

### Step 5: Cleanup — no changes needed

`mtimehx.clear()` (line 412) works the same on `unordered_set`.
`mtimehx.size() > 0` (line 792) also works unchanged, or can become
`!mtimehx.empty()`.

### Step 6: Remove `CompEqual` if no longer used

After this change, `CompEqual` (datarecord.cpp:129-138) and its declaration
(datarecord.h:143-144) are dead code and can be removed.

### Performance comparison

| Operation | `reclist` + `CompEqual` | `unordered_set<mtime_key>` |
|-----------|-------------------------|----------------------------|
| Lookup    | O(n) linear scan        | O(1) amortized hash lookup |
| Insert    | O(1) `push_back`        | O(1) amortized `insert`    |
| Memory    | ~112 bytes/entry (shared_ptr + datarecord) | ~36 bytes/entry (4 scalars + hash bucket) |
| Clear     | O(n) destructor calls   | O(n) but trivial types     |

For an individual generating 50 modeled events, the current approach does
1+2+3+...+50 = 1,275 comparisons total. The `unordered_set` approach does 50
hash lookups — roughly 50x fewer operations.

### Side benefit: unblocks `unique_ptr` / value semantics migration

With `mtimehx` storing value types instead of `rec_ptr`, there is no longer
dual ownership of records between `a[i]` and `mtimehx`. This removes the one
true blocker for migrating from `shared_ptr` to `unique_ptr` or to storing
`datarecord` by value.

---

## Better option: drop pointers entirely, store `datarecord` by value

The `shared_ptr` indirection was originally needed for polymorphism (different
record types via a base class pointer). That polymorphism was removed — there's
now a single `datarecord` class holding all fields. With no inheritance and no
shared ownership, there's no reason to heap-allocate records individually.

### `datarecord` is small and trivially movable

```
int Pos;                    //  4 bytes
unsigned short Evid;        //  2
unsigned short Ss;          //  2
short Cmt;                  //  2 (+2 padding)
unsigned int Addl;          //  4
double Time;                //  8
double Id;                  //  8
double Amt;                 //  8
double Rate;                //  8
double Ii;                  //  8
double Fn;                  //  8
bool Output;                //  1
bool Fromdata;              //  1
bool Lagged;                //  1
bool Armed;                 //  1
                            // ~72 bytes total with padding
```

No virtual functions, no pointers, no heap-allocated members. All scalar types.
The compiler can move/copy this with a single `memcpy`.

### Memory comparison per record

| Approach | Per-record memory | Indirections to access |
|----------|-------------------|----------------------|
| `shared_ptr<datarecord>` | ~112 bytes (object + control block + ptr) | 2 (deque block -> ptr -> object) |
| `unique_ptr<datarecord>` | ~80 bytes (object + ptr) | 2 (deque block -> ptr -> object) |
| `datarecord` by value | ~72 bytes (object only) | 1 (deque block -> object) |

### Why this is better than `unique_ptr`

- **Eliminates a pointer dereference on every access.** Every `thisi[i]->time()`
  becomes `thisi[i].time()`. In `CompRec` (called thousands of times during
  sorts/merges) and in the main simulation loop (673+ iterations), this removes
  one cache miss per access.
- **Better cache locality** — record data is packed directly into deque blocks
  instead of scattered across the heap. Adjacent records in the deque are
  adjacent in memory, so the CPU prefetcher can work effectively.
- **Zero per-record allocation overhead** — no `make_shared`/`make_unique` call
  per record. The deque allocates blocks of records at once.
- **Cheaper moves during sort/insert** — moving a 72-byte trivially-movable
  struct is a `memcpy`. No atomic ops, no pointer management. The compiler can
  auto-vectorize bulk moves during `deque::insert` or `inplace_merge`.
- **Simpler code** — no smart pointer syntax, no `NEWREC` macro needed, no
  move semantics to reason about. Just plain objects.

### What changes in the code

| Current | New |
|---------|-----|
| `typedef std::shared_ptr<datarecord> rec_ptr;` | Drop typedef (or `typedef datarecord& rec_ref;` if convenient) |
| `typedef std::deque<rec_ptr> reclist;` | `typedef std::deque<datarecord> reclist;` |
| `#define NEWREC std::make_shared<datarecord>` | Remove; use constructors directly |
| `thisi.push_back(NEWREC(...))` | `thisi.emplace_back(...)` (construct in place) |
| `thisi[i]->time()` | `thisi[i].time()` |
| `CompRec` uses `a->time()` | `a.time()` |
| Functions taking `rec_ptr` or `rec_ptr&` | Take `datarecord&` or `const datarecord&` |
| `mtimehx` storing `rec_ptr` | Store `mtime_key` value struct (same fix as unique_ptr case) |

### Mid-insert cost consideration

One concern: `deque::insert` in the middle moves actual objects (~72 bytes each)
rather than pointers (~8 bytes each). But `datarecord` is trivially movable, so
the compiler optimizes this to `memmove` on contiguous chunks. Moving 72-byte
trivial objects in contiguous memory is still faster than moving 8-byte
`shared_ptr`s that each require atomic reference count updates. And the deque
mid-insert was already the right choice per PR #1186 — this just makes each
element move cheaper.

### The `offs` vector in `steady_infusion`

With value semantics, the `offs` vector becomes `std::vector<datarecord>`. The
transfer to `thisi` uses `std::move`:

```cpp
for(size_t k = 0; k < offs.size(); ++k) {
    offs[k].time(first_off + double(k)*double(Ii));
    thisi.push_back(std::move(offs[k]));
}
```

`emplace_back` is an alternative that constructs the record directly in the
deque's memory — no temporary, no copy, no move. It forwards its arguments
straight to the `datarecord` constructor:

```cpp
// Option A: move from offs (simple, preserves existing record state)
for(size_t k = 0; k < offs.size(); ++k) {
    offs[k].time(first_off + double(k) * double(Ii));
    thisi.push_back(std::move(offs[k]));
}

// Option B: emplace_back, construct fresh in place
for(size_t k = 0; k < offs.size(); ++k) {
    thisi.emplace_back(Cmt, 9, Amt, first_off + double(k) * double(Ii), Rate, Fn);
}
```

Option B uses the existing constructor (datarecord.h:50):
```cpp
datarecord(short int cmt_, int evid_, double amt_, double time_,
           double rate_, double fn_);
```

Option A is probably clearer — `offs[k]` already has the right `Cmt`, `Amt`,
`Rate`, `Fn` from when it was created, so you're just adjusting the time and
transferring. Option B reconstructs from scratch, which means you need to verify
the constructor arguments match exactly. For a 72-byte trivially-movable struct,
`std::move` is just a `memcpy` anyway, so there's no performance difference.

Note that `offs` still needs to exist as a local working vector — earlier in
`steady_infusion` (lines 360-443), the records in `offs` are used for
`implement()` and timing, and some get consumed via `offs.erase(offs.begin())`
before ever reaching `thisi`. Only the surviving records get transferred in this
final phase.

### Member functions like `schedule()`, `implement()`, `steady()`

These are called on records stored in the deque. With value semantics, the
record is in the deque directly, so `thisi[j].schedule(...)` works. The `this`
pointer inside `schedule()` points to the record in the deque — no change to
the method implementations needed beyond the `->` to `.` syntax for any
`rec_ptr` parameters.

### Bottom line

Dropping pointers entirely is the best path. The class is ~72 bytes of plain
scalars — exactly the kind of object that benefits from value semantics and
contiguous storage. The original motivation for `shared_ptr` (polymorphism) no
longer exists. Going straight to `std::deque<datarecord>` gives you the best
cache behavior, the simplest code, and eliminates all pointer/allocation overhead
in one step.