

/*
Copyright © 2012 kqr. All Rights Reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or other
materials provided with the distribution.

3. The name of the author may not be used to endorse or promote products derived
from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/



#include "safelist_extra.h"


List *list(int argc, ...) {
  List *temp = init();
  int i;
  va_list ap;

  va_start(ap, argc);
  for (i = 0; i < argc; i++) {
    temp = cons(va_arg(ap, double), temp);
  }
  va_end(ap);

  return temp;
}


List *cons_array(double vals[], int length) {
  List *temp = init();

  while (length-->0) {
    temp = cons(vals[length], temp);
  }

  return temp;
}


List *append(List *xs, List *ys) {
  if (null(xs)) {
    return ys;
  } else {
    return cons(first(xs), append(rest(xs), ys));
  }
}


int length(List *xs) {
  if (null(xs))
    return 0;
  else
    return 1 + length(rest(xs));
}


List *reversea(List *xs, List *rev) {
  if (null(xs)) {
    return rev;
  } else {
    double head = first(ref(xs));
    return reversea(rest(xs), cons(head, rev));
  }
}
List *reverse(List *xs) {
  return reversea(xs, init());
}


double nth(int n, List *xs) {
  if (n <= 0)
    return first(xs);
  else
    return nth(n-1, rest(xs));
}


List *take(int n, List *xs) {
  double head = 0;

  if (null(xs) || n <= 0) {
    return init();
  } else {
    head = first(ref(xs));
    return cons(head, take(n-1, rest(xs)));
  }
}


List *drop(int n, List *xs) {
  if (null(xs)) {
    return init();
  } else if (n <= 0) {
    return xs;
  } else {
    return drop(n-1, rest(xs));
  }
}



List *map(double (*f)(double), List *xs) {
  double head = 0;

  if (null(xs)) {
    return init();
  } else {
    head = first(ref(xs));
    return cons((*f)(head), map(f, rest(xs)));
  }
}


List *filter(int (*f)(double), List *xs) {
  double head = 0;

  if (null(xs)) {
    return init();
  } else {
    head = first(ref(xs));
    if ((*f)(head)) {
      return cons(head, filter(f, rest(xs)));
    } else {
      return filter(f, rest(xs));
    }
  }

}






