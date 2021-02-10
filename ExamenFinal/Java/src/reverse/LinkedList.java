package reverse;

import java.util.StringJoiner;

public class LinkedList<T> {

    private static class Node<E> {
        E elem;
        Node<E> next;

        Node(E elem) {
            this.elem = elem;
            this.next = null;
        }
    }

    private Node<T> first;

    // DO NOT MODIFY CODE ABOVE
    // Please, fill in your data
    //
    // Surname, Name: Rey Leyva, Alejandro
    // Group: Matemáticas + Informática

    public void reverse() {
        Node<T> current = first;
        Node<T> aux;
        Node<T> previous = null;
        while(current!=null){
            aux=current;
            current=current.next;
            aux.next=previous;
            previous=aux;
        }
        first=previous;
    }

    // DO NOT MODIFY CODE BELOW

    public static LinkedList<Integer> testList() {
        LinkedList<Integer> list = new LinkedList<>();
        Node<Integer> node = new Node<>(0);
        list.first = node;
        for (int i = 1; i < 10; i++) {
            node.next = new Node<>(i);
            node = node.next;
        }
        return list;
    }

    @Override
    public String toString() {
        StringJoiner sj = new StringJoiner(",", "LinkedList(", ")");
        for (Node<T> node = first; node != null; node = node.next)
            sj.add(node.elem.toString());
        return sj.toString();
    }
}
