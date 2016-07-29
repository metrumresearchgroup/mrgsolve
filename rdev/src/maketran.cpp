
// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

//#include <Rcpp.h>
#include "RcppInclude.h"



// [[Rcpp::export]]
Rcpp::NumericMatrix EXPAND_EVENTS(Rcpp::IntegerVector idcol_,
                                  Rcpp::NumericMatrix events,
                                  Rcpp::NumericVector id) {
  
  int i,j,k;
  int crow = 0;
  
  int idcol = idcol_[0]-1;
  int ncol_new = events.ncol();
  
  Rcpp::List dimnames = events.attr("dimnames");
  Rcpp::CharacterVector names = dimnames[1];
  
  if(idcol < 0) {
    ncol_new = events.ncol() + 1;  
    names.push_back("ID");
    idcol = ncol_new-1;
    dimnames[1] = names;
  } 

  Rcpp::NumericMatrix ans(events.nrow()*id.size(),ncol_new);
  
  for(i=0; i < id.size(); i++) {
    for(j=0; j < events.nrow(); j++) {
      for(k=0; k < events.ncol(); k++) {
        ans(crow,k) = events(j,k);
      }
      ans(crow,idcol) = id[i];
      crow++;
    }
  }
  dimnames[0]  = Rcpp::CharacterVector(0);
  ans.attr("dimnames") = dimnames;
  return(ans);
}





// Rcpp::NumericMatrix EXPAND_BASE(Rcpp::List parin,
// 			     Rcpp::NumericVector stimes,
// 			     Rcpp::NumericVector id) {
//   int i,j;
//   int ntime = stimes.size();
//   int nid  = id.size();
//   int ncol = Rcpp::as<int>(parin["ncol"]);
//   Rcpp::NumericMatrix ans(ntime*nid, ncol);

//   int crow = 0;
//   for(i=0; i < nid; i++) {
//     for(j=0; j < ntime; j++) {
//       ans(crow, 0) = stimes[j];
//       ans(crow,1) = id[i];
//       ++crow;
//     }
//   }
//   return(ans);
// }



// Rcpp::NumericMatrix START_SKELE(Rcpp::List parin,
// 				Rcpp::NumericVector stimes,
// 				Rcpp::NumericVector etimes,
// 				Rcpp::NumericVector id,
// 				Rcpp::NumericVector eid,
// 				Rcpp::IntegerVector evid) {
//   int i,j;
//   int ncol = Rcpp::as<int>(parin["ncol"]);
//   int recycle = Rcpp::as<int>(parin["recycle"]);

//   int nout = stimes.size()*id.size();
//   if(recycle==1) {
//     nout = nout + id.size()*etimes.size();
//   }
//   if(recycle==0) {
//     nout  = nout + etimes.size();
//   }
//   Rcpp::NumericMatrix ans(nout,ncol);
//   int crow = 0;
//   for(i=0; i < id.size(); i++) {
//     for(j=0; j < stimes.size(); j++) {
//       ans(crow,0) = -1; // rown
//       ans(crow,1) = id[i];  // id
//       ans(crow,2) = stimes[j];  // time
//       ans(crow,3) = 0;  // evid
//       crow++;

//     }
//   }

//   // We are receiving  a single events data frame
//   if(recycle==1) {
//     for(i=0; i < id.size(); i++) {
//       for(j=0; j < etimes.size(); j++) {
//   	ans(crow,0) = j; // rown
//   	ans(crow,1) = id[i]; // id
//   	ans(crow,2) = etimes[j]; // time
// 	ans(crow,3) = evid[j];  // evid
//   	crow++;
//       }
//     }
//   }
//   // Complete specification of events for each id:
//   if(recycle==0) {
//     for(i=0; i < eid.size(); i++) {
//       ans(crow,0) = i;
//       ans(crow,1) = eid[i];
//       ans(crow,2) = etimes[i];
//       ans(crow,3) = evid[i];
//       crow++;
//     }
//   } // End recycle

//   return(ans);
// }



// Rcpp::NumericMatrix FINISH_SKELE(Rcpp::List parin,
// 				 Rcpp::NumericMatrix events,
// 				 Rcpp::NumericMatrix base,
// 				 Rcpp::IntegerVector rown,
// 				 Rcpp::IntegerVector ord,
// 				 Rcpp::IntegerVector copy_pos) {
//   int i,j;

//   Rcpp::NumericMatrix ans(base.nrow(), events.ncol());
//   // base is rown, id,time
//   //  int crow = 0;
//   for(i=0; i < base.nrow(); i++) {
//     ans(i,0)  = base(ord[i],1);
//     ans(i,1) =  base(ord[i],2);
//     ans(i,2) =  base(ord[i],3);
//     if(rown[ord[i]] < 0) continue;
//     for(j=0; j < copy_pos.size(); j++) {
//       ans(i,3+j) = events(rown[ord[i]],copy_pos[j]);
//     }
//   }
//   return(ans);
// }




typedef std::vector<std::string> svec;


// Rcpp::NumericMatrix SIMSKELE(Rcpp::List parin,
// 			     Rcpp::NumericMatrix events,
// 			     Rcpp::NumericVector stimes,
// 			     Rcpp::NumericVector id) {
//   int recycle=0;

//   int ncol = Rcpp::as<int>(parin["ncol"]);
//   Rcpp::List dimnames = events.attr("dimnames");
//   Rcpp::CharacterVector ecols = dimnames[1];
//   Rcpp::CharacterVector::iterator it;
//   if(it == ecols.end()) recycle = 1;

//   int nout = stimes.size()*id.size();

//   if(recycle==1) {
//     nout = nout + id.size()*events.nrow();
//   }
//   if(recycle==0) {
//     nout  = nout + events.nrow();
//   }
//   Rcpp::NumericMatrix ans(nout,ncol);

//   return(ans);
// }







// Rcpp::List EXPAND(Rcpp::List parin,
// 		  Rcpp::NumericMatrix data,
// 		  Rcpp::NumericMatrix batch,
// 		  Rcpp::NumericVector stimes,
// 		  Rcpp::Function fun) {

//   int i, j;
//   Rcpp::NumericVector ID;
//   if(data.nrow()>0) {
//     ID = data(Rcpp::_, 0);
//   } else {
//     ID = batch(Rcpp::_, 0);
//   }
//   Rcpp::List dimnames = data.attr("dimnames");
//   Rcpp::CharacterVector colnames = dimnames[1];

//   Rcpp::CharacterVector find;
//   find.push_back("evid");
//   Rcpp::IntegerVector evidcol = Rcpp::match(find,colnames);

//   Rcpp::NumericVector UID = unique(ID);
//   std::sort(UID.begin(),UID.end());

//   int ntime = stimes.size();
//   int nid = UID.size();
//   int nr = nid*ntime + data.nrow();

//   Rcpp::NumericMatrix dat(nr, data.ncol());
//   int count = 0;
//   for(i=0; i < nid; i++) {
//     for(j=0; j < ntime; j++) {
//       dat(count, 0) = UID[i];
//       dat(count,1) = stimes[j];
//       ++count;
//     }
//   }


//   if(data.nrow()>0) {
//     for(i=0; i < data.nrow(); i++) {
//       for(j=0; j < data.ncol(); j++) {
// 	dat(count,j) = data(i,j);
//       }
//       count++;
//     }
//   }

//   dat = fun(dat,1,2,evidcol[0]);

//   Rcpp::List ans;
//   ans["data"] = dat;

//   return(ans);
// }






