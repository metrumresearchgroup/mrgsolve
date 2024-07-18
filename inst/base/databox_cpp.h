// Copyright (C) 2013 - 2023  Metrum Research Group
//
// This file is part of mrgsolve.
//
// mrgsolve is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// mrgsolve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


#ifndef DATABOX_CPP_H
#define DATABOX_CPP_H

void databox::mevent(double time, int evid) {
  mrgsolve::evdata ev(time,evid);
  ev.check_unique = true;
  mevector.push_back(ev);
}

double databox::mtime(double time) {
  mrgsolve::evdata ev(time,2);
  ev.check_unique = true;
  mevector.push_back(ev);
  return time;
}

/**
 * Calculates time after dose.
 * 
 * @return the calculated time after dose; if no dose has been given for the 
 * current individual, then -1 is returned.
 * 
 */
double databox::tad() {
  static double told = -1.;
  if(newind <= 1) told = -1.0;
  if((evid == 1) || (evid == 4)) told = time;
  return told < 0 ? -1.0 : time - told;
}

void databox::push(mrgsolve::evdata x) {
  mevector.push_back(x); 
  return;
}

#endif
