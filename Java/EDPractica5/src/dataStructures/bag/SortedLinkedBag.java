/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 5.
 * Ejercicio 12 de la tercera relación. Implementar el TAD Bolsa
 *
 * Alumno: Rey Leyva, Alejandro
 ********************************************************************/

package dataStructures.bag;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;

public class SortedLinkedBag<T extends Comparable<? super T>> implements Bag<T> {

    static private class Node<E> {
        private E elem;
        private int count;
        private Node<E> next;

        Node(E x, int n, Node<E> node) {
            elem = x;
            count = n;
            next = node;
        }
    }

    /**
     * Invariant:
     * <p>
     * 1. keep the linked list sorted by "elem", with no duplicates
     * <p>
     * 2. the counter "count" must be positive
     * <p>
     * Example: first -> ('a', 5) -> ('d', 1) -> ('t', 3) -> ('z', 2)
     */
    private Node<T> first;

    public SortedLinkedBag() {
        first = null;
    }

    @Override
    public boolean isEmpty() {
        return first == null;
    }

    @Override
    public void insert(T item) {
        if (this.isEmpty()) this.first = new Node<>(item, 1, first);
        else {
            Node<T> nodoActual = first;
            Node<T> nodoPrevio = null;

            while (nodoActual != null && item.compareTo(nodoActual.elem) > 0) {
                nodoPrevio = nodoActual;
                nodoActual = nodoActual.next;
            }

            if (nodoActual != null && item.compareTo(nodoActual.elem) == 0) {
                nodoActual.count++;
            }

            else if (nodoPrevio != null){
                nodoPrevio.next = new Node<>(item, 1, nodoActual);
            }
        }
    }

    @Override
    public int occurrences(T item) {
        Node<T> nodo = first;

        while (nodo != null && !nodo.elem.equals(item)) {
            nodo = nodo.next;
        }

        if (nodo == null) return 0;
        else return nodo.count;
    }

    @Override
    public void delete(T item) {
        Node<T> nodoActual = first;
        Node<T> nodoPrevio = null;

        while (nodoActual != null && !nodoActual.elem.equals(item)) {
            nodoPrevio = nodoActual;
            nodoActual = nodoActual.next;
        }

        if (nodoActual != null && nodoActual.elem.equals(item)) {
            if (nodoActual.count > 1) {
                nodoActual.count--;
            } else if (nodoPrevio != null){
                nodoPrevio.next = nodoActual.next;
            } else {
                first = nodoActual.next;
            }
        }
    }

    @Override
    public void copyOf(Bag<T> source) {
        // TODO
        // you cannot use insert on 'this'
    }

    @Override
    public String toString() {
        StringJoiner res = new StringJoiner(" ", " Bag [", "]");
        for (Node<T> node = first; node != null; node = node.next) {
            res.add("(" + node.elem + ", " + node.count + ")");
        }
        return res.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return new SortedLinkedBagIterator();
    }

    private class SortedLinkedBagIterator implements Iterator<T> {
        Node<T> node;

        public SortedLinkedBagIterator() { node = first; }

        public boolean hasNext() {
            return node != null;
        }

        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }

            T currentElem = node.elem;
            node = node.next;
            return currentElem;
        }
    }
}
