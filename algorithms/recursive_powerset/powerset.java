// Java
// real    0m12.326s

import java.util.LinkedList;
import java.util.Iterator;

public class Powerset {
    private static <T> LinkedList<LinkedList<T>> powerset(LinkedList<T> set) {
        LinkedList<T> remaining = (LinkedList<T>) set.clone();
        LinkedList<LinkedList<T>> subsets = new LinkedList<LinkedList<T>>();
        subsets.add(new LinkedList<T>());

        Iterator<T> it = remaining.iterator();
        while (it.hasNext()) {
            T elem = it.next();
            it.remove();

            LinkedList<LinkedList<T>> subpowersets = powerset(remaining);

            for (LinkedList<T> subpowerset : subpowersets) {
                subpowerset.add(elem);
                subsets.add(subpowerset);
            }
        }

        return subsets;
    }

    public static void main(String[] args) {
        int max = 21;
        LinkedList<Integer> testset = new LinkedList<Integer>();

        for (int i = 1; i <= max; i++)
          testset.add(i);

        System.out.println("" + powerset(testset).size());
    }
}