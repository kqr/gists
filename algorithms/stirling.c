/*
 * Computes the stirling number S(n, k). Runs in O(n) memory and O(n*k) time
 */


double stirling(int n, int k) {
    double *generation;
    double result;
    int i, j;

    if ((generation = calloc(k+1, sizeof(double))) == NULL) {
        printf("Calloc failed!\n");
        return -1;
    }

    generation[0] = 1;
    for (i = 1; i <= n; i++) {
        for (j = min(i, k); j > 0; j--) {
            generation[j] = j*generation[j] + generation[j-1];
        }
        generation[0] = 0;
    }

    result = generation[k];
    free(generation);
    return result;
}
 
