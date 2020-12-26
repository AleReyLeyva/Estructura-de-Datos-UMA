/**
 * A simple test driver for the Bag ADT.
 * <p>
 * Tests for mostFrequent, iterator and copyOf.
 */

import dataStructures.bag.Bag;
import dataStructures.bag.SortedLinkedBag;

import static dataStructures.bag.BagUtils.mostFrequent;

public class BagUtilsDemo {

    public static void main(String[] args) {

        Bag<Character> empty = new SortedLinkedBag<>();

        Bag<Character> singleton = new SortedLinkedBag<>();
        singleton.insert('S');

        String cervantes = "En un lugar de la Mancha";
        String text = cervantes;
        Bag<Character> bag = new SortedLinkedBag<>();

        // 1. feed the bag with the text
        for (int i = 0; i < text.length(); i++) {
            if (Character.isAlphabetic(text.charAt(i))) {
                bag.insert(text.charAt(i));
            }
        }

        // test mostFrequent

        System.out.println("Testing mostFrequent()");
        System.out.println("empty = " + empty);
        System.out.println("most frequent char = " + mostFrequent(empty));

        System.out.println("singleton = " + singleton);
        System.out.println("most frequent char = " + mostFrequent(singleton));

        System.out.println("original = " + bag);
        System.out.println("most frequent char = " + mostFrequent(bag));
        System.out.println("after inserting an n");
        bag.insert('n');
        System.out.println("most frequent char = " + mostFrequent(bag));
        System.out.println("the extra occurrence of n is deleted");
        bag.delete('n');
        System.out.println();

        // test bag iterator

        System.out.println("Testing the Bag iterator");
        System.out.print("contents of the empty bag: ");
        for (char c : empty) {
            System.out.print(c + " ");
        }
        System.out.println();

        System.out.print("contents of the singleton bag: ");
        for (char c : singleton) {
            System.out.print(c + " ");
        }
        System.out.println();

        System.out.print("contents of the sample bag: ");
        for (char c : bag) {
            System.out.print(c + " ");
        }
        System.out.println();
        System.out.println();

        // test copyOf

        System.out.println("Testing copyOf()");
        Bag<Character> copy = new SortedLinkedBag<>();

        copy.copyOf(empty);
        System.out.println("copy of the empty bag: " + copy);

        copy.copyOf(singleton);
        System.out.println("copy of the singleton bag: " + copy);

        copy.copyOf(bag);
        System.out.println("copy of the sample bag : " + copy);
        System.out.println("sample bag not modified: " + bag);
        System.out.println("the sample bag and its copy are not aliased:");
        bag.insert('d');
        System.out.println("sample bag:" + bag);
        System.out.println("copy bag:  " + copy);

        copy.copyOf(copy);
        System.out.println("auto-copy of the copy bag: " + copy);
        System.out.println();
    }
}
