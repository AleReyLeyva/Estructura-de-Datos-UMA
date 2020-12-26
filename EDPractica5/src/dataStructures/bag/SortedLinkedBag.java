/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 5.
 * Ejercicio 12 de la tercera relación. Implementar el TAD Bolsa
 *
 * Alumno: APELLIDOS, NOMBRE
 ********************************************************************/

package dataStructures.bag;

import java.util.Iterator;

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
        // TODO
    }

    @Override
    public boolean isEmpty() {
        // TODO
        return false;
    }

    @Override
    public void insert(T item) {
        // TODO
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
