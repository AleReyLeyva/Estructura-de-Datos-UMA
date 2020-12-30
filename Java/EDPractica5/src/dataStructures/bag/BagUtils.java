package dataStructures.bag;

import java.util.Iterator;

public class BagUtils {

    /**
     * Returns the most frequent element in the bag {@code b}. If there are several
     * elements with the same number of occurrences, returns the maximum of them. If
     * the bag is empty returns {@code null} (which is usually a bad idea).
     *
     * @param b the bag to scan for the most frequent element
     * @return the most frequent element in {@code b}
     */
    public static <T extends Comparable<? super T>> T mostFrequent(Bag<T> b) {
        Iterator<T> it = b.iterator();
        T res = it.hasNext() ? it.next() : null;

        while (it.hasNext()) {
            T next = it.next();
            if (b.occurrences(next) > b.occurrences(res)) { res = next; }
            else if (b.occurrences(next) == b.occurrences(res)) {
                res = next.compareTo(res) > 0 ? next : res;
            }
        }
        return res;
    }
}
