
#include <stdio.h>
#include <string.h>
#include "safelist_core.h"
#include "safelist_extra.h"


double square(double x) {
  return x*x;
}

int lessthanfifty(double x) {
  return x < 50;
}


int main(int argc, char *argv[]) {
  List *lista = init();
  double fat_array[] = {58, 25, 89, 11, 71, 3, 38, 49, 4, 16, 13, 13, 97, 80, 37, 92, 17, 88, 77, 80, 21, 70, 81, 2};

  printf("Making a new list of a long array.\n");
  lista = cons_array(fat_array, (int) (sizeof(fat_array)/sizeof(double)));
  printf("first: %f\n", first(ref(lista)));
  printf("second: %f\n", first(rest(ref(lista))));
  printf("third: %f\n", first(rest(rest(ref(lista)))));
  printf("nth 4: %f\n", nth(4, ref(lista)));
  printf("length: %i\n", length(ref(lista)));

  printf("Clearing the list by dereffing it.\n\n");
  deref(&lista);

  printf("Creating a new list by means of variable number of arguments (!)\n");
  lista = list(3, 3.14159, 1.618, 1.414213);
  printf("length: %i\n", length(ref(lista)));
  printf("[%f, %f, %f]\n\n", nth(0, ref(lista)), nth(1, ref(lista)), nth(2, ref(lista)));


  deref(&lista);
  printf("New list!! (consing lol.)\n");
  lista = cons(1, cons(2, cons(3, cons(4, cons(5, lista)))));
  printf("[%f, %f, %f, %f, %f]\n", nth(0, ref(lista)), nth(1, ref(lista)), nth(2, ref(lista)), nth(3, ref(lista)), nth(4, ref(lista)));
  printf("Mapping x^2 over lista...\n");
  map(square, lista);
  printf("[%f, %f, %f, %f, %f]\n\n", nth(0, ref(lista)), nth(1, ref(lista)), nth(2, ref(lista)), nth(3, ref(lista)), nth(4, ref(lista)));

  lista = reverse(lista);
  printf("List reversed!\n");
  printf("[%f, %f, %f, %f, %f]\n\n", nth(0, ref(lista)), nth(1, ref(lista)), nth(2, ref(lista)), nth(3, ref(lista)), nth(4, ref(lista)));

  lista = take(4, lista);
  printf("Taking 4: [%f, %f, %f, %f]\n", nth(0, ref(lista)), nth(1, ref(lista)), nth(2, ref(lista)), nth(3, ref(lista)));
  lista = drop(2, lista);
  printf("Dropping 2: [%f, %f]\n\n", nth(0, ref(lista)), nth(1, ref(lista)));

  deref(&lista);
  printf("Ny lång lista igen, från samma array!!\n");
  lista = cons_array(fat_array, (int) (sizeof(fat_array)/sizeof(double)));
  printf("Filtrerar ut element lägre än 50.\n");
  lista = filter(lessthanfifty, lista);
  printf("first: %f\n", first(ref(lista)));
  printf("second: %f\n", first(rest(ref(lista))));
  printf("third: %f\n", first(rest(rest(ref(lista)))));
  printf("nth 4: %f\n", nth(4, ref(lista)));
  printf("length: %i\n\n", length(ref(lista)));


  deref(&lista);
  printf("Address for dereffed lista: %X\n", (unsigned int) lista);

  return 0; 
}

