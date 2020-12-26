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
        this.value = new T[INITIAL_CAPACITY];
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

        }
    }

    @Override
    public int occurrences(T item) {
        // TODO
        return 0;
    }

    @Override
    public void delete(T item) {
        // TODO
    }

    @Override
    public void copyOf(Bag<T> source) {
        // TODO
        // you cannot use insert on 'this'
    }

    @Override
    public String toString() {
        // TODO
        return null;
    }

    @Override
    public Iterator<T> iterator() {
        // TODO
        return null;
    }
}
