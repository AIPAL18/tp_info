int partition(int tab[], int n) {
    int pivot = tab[n - 1];
    int i = -1;
    for (int j = 0; j < n; j++) {
        if (tab[j] < pivot) {
            i++;
            swap(tab, i, j);
        }
    }
    swap(tab, i+1, n-1);
    return i+1;
}