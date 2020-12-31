/********************************************************************
 * Estructuras de Datos. 2º ETSI Informática. UMA
 * PRACTICA 5.
 * Ejercicio 12 de la tercera relación. Implementar el TAD Bolsa.
 *
 * Alumno: Rey Leyva, Alejandro
 ********************************************************************/

package dataStructures.bag;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;

public class SortedArrayBag<T extends Comparable<? super T>> implements Bag<T> {

    private final static int INITIAL_CAPACITY = 1;

    // The bag is represented by two arrays ("value" and "count") and
    // a cursor ("nextFree").
    //
    // The bag is stored in the first "nextFree-1" positions of
    // arrays "value" and "count". Cursor "nextFree" refers to the
    // first position available in both arrays (if any).
    //
    // The values are stored sorted in the array "value", the counter
    // corresponding to "value[i]" is stored in "counter[i]"; thus
    // the bag { ('a', 6), ('d', 2), ('t', 7)} is represented by:
    //
    // value = { 'a', 'd', 't' }
    // count = { 6 , 2 , 7 }
    // nextFree = 3
    //
    // The remaining positions in arrays "value" and "count" are
    // undefined (most likely null and zero, respectively).

    private T[] value; // keep this array sorted
    private int[] count; // keep only positive counters
    private int nextFree;

    public SortedArrayBag() {
        this.value = (T[]) new Comparable[INITIAL_CAPACITY];
        this.count = new int[INITIAL_CAPACITY];
        this.nextFree = 0;
    }

    private void ensureCapacity() {
        if (nextFree == value.length) {
            this.value = Arrays.copyOf(value, value.length * 2);
            this.count = Arrays.copyOf(count, count.length * 2);
        }
    }

    @Override
    public boolean isEmpty() {
        return nextFree == 0;
    }

    // search loop:
    // if "item" is stored in the array "value", returns its index;
    // otherwise returns the index where "item" would be inserted

    private int locate(T item) {
        int lower = 0;
        int upper = nextFree - 1;
        int mid = 0;
        boolean found = false;

        // binary search
        while (lower <= upper && !found) {
            mid = lower + ((upper - lower) / 2); // == (lower + upper) / 2;
            found = value[mid].equals(item);
            if (!found) {
                if (value[mid].compareTo(item) > 0) {
                    upper = mid - 1;
                } else {
                    lower = mid + 1;
                }
            }
        }

        if (found)
            return mid; // the index where "item" is stored
        else
            return lower; // the index where "item" would be inserted
    }

    @Override
    public void insert(T item) {
        // Aseguramos un hueco para el nuevo elemento en caso de que no esté en la bolsa
        ensureCapacity();

        int index = locate(item);

        // Comprobamos si el item está o no está en la bolsa

        if (value[index] != null && value[index].equals(item)) {
            count[index]++;
        } else {
            for (int i=nextFree; i>index; i--) {
                value[i] = value[i-1];
                count[i] = count[i-1];
            }
            value[index] = item;
            count[index] = 1;

            nextFree++;
        }
    }

    @Override
    public int occurrences(T item) {
        int index = locate(item);

        if (value[index] != null && value[index].equals(item)) return count[index];
        else return 0;
    }

    @Override
    public void delete(T item) {
        int index = locate(item);

        if (value[index] != null && value[index].equals(item)) {
            if (count[index] > 1) {
                count[index]--;
            } else {
                for (int i=index; i<value.length-1; i++) {
                    value[i] = value[i+1];
                    count[i] = count[i+1];
                }
                nextFree--;
            }
        }
    }

    @Override
    public void copyOf(Bag<T> source) {
        SortedArrayBag<T> copy = (SortedArrayBag<T>) source;
        this.value = copy.value;
        this.count = copy.count;
        this.nextFree = copy.nextFree;
    }

    @Override
    public String toString() {
        StringJoiner res = new StringJoiner(" ", " Bag [", "]");
        for (int i=0; i<value.length; i++) {
            res.add("(" + value[i] + ", " + count[i] + ")");
        }
        return res.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return new SortedArrayBagIterator();
    }

    private class SortedArrayBagIterator implements Iterator<T> {
        int index;

        public SortedArrayBagIterator() { index = 0; }

        public boolean hasNext() { return index < nextFree; }

        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }

            index++;
            return value[index-1];
        }
    }
}
