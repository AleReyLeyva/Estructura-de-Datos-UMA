package huffman;

/*
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name: Ale Rey Leyva
 * Student's group: Matemáticas + Informática
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> weights = new AVLDictionary<>();
        for (int i=0; i<s.length(); i++) {
            Character key = s.charAt(i);
            Integer value = weights.valueOf(key);

            if (value == null) {
                weights.insert(key, 1);
            } else {
                weights.insert(key, value+1);
            }
        }
        return weights;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	PriorityQueue<WLeafTree<Character>> res = new BinaryHeapPriorityQueue<>();
    	Dictionary<Character, Integer> weights = weights(s);
        Iterator<Tuple2<Character, Integer>> it = weights.keysValues().iterator();

        while (it.hasNext()) {
            Tuple2<Character, Integer> next = it.next();
            WLeafTree<Character> node = new WLeafTree<>(next._1(), next._2());
            res.enqueue(node);
        }
        return res;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        WLeafTree<Character> res;
        Dictionary<Character, Integer> weights = weights(s);
        if (weights.size() < 2) {
            throw new HuffmanException("La cadena tiene que tener al menos 2 caracteres distintos.");
        } else {
            PriorityQueue<WLeafTree<Character>> queue = huffmanLeaves(s);
            WLeafTree<Character> first = queue.first();
            queue.dequeue();
            WLeafTree<Character> next = queue.first();
            queue.dequeue();
            res = new WLeafTree<>(first, next);
            while (!queue.isEmpty()) {
                next = queue.first();
                queue.dequeue();
                res = new WLeafTree<>(next, res);
            }
        }
    	return res;
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
        Iterator<Tuple2<Character, List<Integer>>> it = d1.keysValues().iterator();
        while (it.hasNext()) {
            Tuple2<Character, List<Integer>> next = it.next();
            res.insert(next._1(), next._2());
        }
        it = d2.keysValues().iterator();
        while (it.hasNext()) {
            Tuple2<Character, List<Integer>> next = it.next();
            res.insert(next._1(), next._2());
        }
    	return res;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
        Iterator<Tuple2<Character, List<Integer>>> it = d.keysValues().iterator();
        while (it.hasNext()) {
            Tuple2<Character, List<Integer>> next = it.next();
            List<Integer> newValue = next._2();
            newValue.prepend(i);
            res.insert(next._1(), newValue);
        }
        return res;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {

        Dictionary<Character, List<Integer>> leftChild = new AVLDictionary<>();
        Dictionary<Character, List<Integer>> rightChild = new AVLDictionary<>();

        if (ht.leftChild().isLeaf()) {
            List<Integer> cero = new LinkedList<>();
            cero.append(0);
            leftChild.insert(ht.leftChild().elem(), cero);
        } else {
            leftChild = prefixWith(0, huffmanCode(ht.leftChild()));
        }

        if (ht.rightChild().isLeaf()) {
            List<Integer> uno = new LinkedList<>();
            uno.append(1);
            rightChild.insert(ht.rightChild().elem(), uno);
        } else {
            rightChild = prefixWith(1, huffmanCode(ht.rightChild()));
        }

        return joinDics(leftChild, rightChild);
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        List<Integer> res = new ArrayList<>();
        Iterator<Integer> it;
        for (int i=0; i<s.length(); i++) {
            Character key = s.charAt(i);
            List<Integer> values = hc.valueOf(key);
            it = values.iterator();
            while (it.hasNext()) {
                res.append(it.next());
            }
        }
        return res;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        return decodeRec(0, bits, ht, ht);
    }

    private static String decodeRec(int i, List<Integer> bits, WLeafTree<Character> ht, WLeafTree<Character> parcial) {
        if (i==bits.size()) {
            return "";
        } else {
            int bit = bits.get(i);
            if (bit == 0 && parcial.leftChild().isLeaf()) {
                return parcial.leftChild().elem() + decodeRec(i+1, bits, ht, ht);
            } else if (bit == 0){
                return decodeRec(i+1, bits, ht, parcial.leftChild());
            }

            if (bit == 1 && parcial.rightChild().isLeaf()) {
                return parcial.rightChild().elem() + decodeRec(i+1, bits, ht, ht);
            } else {
                return decodeRec(i+1, bits, ht, parcial.rightChild());
            }
        }
    }
}
