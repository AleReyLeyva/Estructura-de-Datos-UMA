/**
 * A simple test driver for the Bag ADT.
 * <p>
 * Iterator and copyOf are not tested.
 */

import dataStructures.bag.Bag;
import dataStructures.bag.SortedLinkedBag;

public class BagTester {

    public static void main(String[] args) {

        Bag<Character> b = new SortedLinkedBag<>();

        String cervantes = "En un lugar de la Mancha, de cuyo nombre no quiero acordarme, "
                + "no ha mucho tiempo que vivía un hidalgo de los de lanza en astillero, "
                + "adarga antigua, rocín flaco y galgo corredor. Una olla de algo más vaca "
                + "que carnero, salpicón las más noches, duelos y quebrantos los sábados, "
                + "lentejas los viernes, algún palomino de añadidura los domingos, consumían "
                + "las tres partes de su hacienda.";

        String dickens = "IT WAS the best of times, it was the worst of times,"
                + "it was the age of wisdom, it was the age of foolishness,"
                + "it was the epoch of belief, it was the epoch of incredulity,"
                + "it was the season of Light, it was the season of Darkness,"
                + "it was the spring of hope, it was the winter of despair,"
                + "we had everything before us, we had nothing before us,"
                + "we were all going direct to Heaven, we were all going direct the other way"
                + "- in short, the period was so far like the present period, that some of"
                + " its noisiest authorities insisted on its being received,"
                + "for good or for evil, in the superlative degree of comparison only.";

        // choose text for the test
        String text;
        // text = cervantes;
        text = dickens;

        System.out.println(b.isEmpty());

        // 1. feed the bag with the text
        for (int i = 0; i < text.length(); i++) {
            b.insert(text.charAt(i));
        }
        System.out.println(b);
        printVowels(b);

        // 2. remove some characters
        String toBeDeleted = "e Muy aaa cuccu monk java ghci duEee";
        for (int i = 0; i < toBeDeleted.length(); i++) {
            b.delete(toBeDeleted.charAt(i));
        }
        System.out.println(b);
        printVowels(b);

        // 3. remove non letters left
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (!Character.isLetter(c)) {
                removeAll(b, c);
            }
        }
        System.out.println(b);
        printVowels(b);

        // 4. remove all occurrences of some letters
        String toBeNuked = "zywvu";
        for (int i = 0; i < toBeNuked.length(); i++) {
            removeAll(b, toBeNuked.charAt(i));
        }
        System.out.println(b);
        printVowels(b);

        // 5. remove vowels left
        removeVowels(b);
        printVowels(b);

        System.out.println(b.isEmpty());
    }

    public static <T extends Comparable<? super T>> void removeAll(Bag<T> b, T c) {
        while (b.occurrences(c) > 0) {
            b.delete(c);
        }
    }

    public static void printVowels(Bag<Character> b) {
        final String vowels = "aeiou";
        for (int i = 0; i < vowels.length(); i++) {
            char vowel = vowels.charAt(i);
            System.out.println(vowel + " occurrs " + b.occurrences(vowel) + " times");
        }
    }

    public static void removeVowels(Bag<Character> b) {
        final String vowels = "aeiou";
        for (int i = 0; i < vowels.length(); i++) {
            removeAll(b, vowels.charAt(i));
        }
    }
}
