package dataStructures.dictionary;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre: Rey Leyva, Alejandro
 * Titulacion, Grupo: Matematicas + Informatica
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
	    bKeys = new HashDictionary<>();
	    bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
	    return bKeys.isEmpty() && bValues.isEmpty();
	}
	
	public int size() {
	    return bKeys.size();
	}
	
	public void insert(K k, V v) {
	    bKeys.insert(k, v);
	    bValues.insert(v, k);
	}
	
	public V valueOf(K k) {
	    if (!bKeys.isDefinedAt(k))
	        return null;
	    else
	        return bKeys.valueOf(k);
	}
	
	public K keyOf(V v) {
        if (!bValues.isDefinedAt(v))
            return null;
        else
            return bValues.valueOf(v);
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
        bValues.delete(bKeys.valueOf(k));
	    bKeys.delete(k);
	}
	
	public void deleteByValue(V v) {
        bKeys.delete(bValues.valueOf(v));
        bValues.delete(v);
	}
	
	public Iterable<K> keys() {
		return bKeys.keys();
	}
	
	public Iterable<V> values() {
		return bValues.keys();
	}
	
	public Iterable<Tuple2<K, V>> keysValues() {
		return bKeys.keysValues();
	}
	
		
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K,V> dict) {
	    if (!esInyectivo(dict)) {
	        throw new IllegalArgumentException();
        } else {
	        BiDictionary<K,V> res = new HashBiDictionary<>();
	        Iterator<Tuple2<K,V>> it = dict.keysValues().iterator();
	        while (it.hasNext()) {
	            Tuple2<K,V> tuple = it.next();
	            res.insert(tuple._1(), tuple._2());
            }
            return res;
        }
	}

    private <V extends Comparable<? super V>, K> boolean esInyectivo(Dictionary<K,V> dict) {
	    Iterator<V> values = dict.values().iterator();
	    int i = 0;
	    while (values.hasNext()) {
	        V v = values.next();
	        i++;
        }
	    return i == dict.size();
    }
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
	    BiDictionary<K,W> res = new HashBiDictionary<>();
	    Iterator<K> itKey = this.bKeys.keys().iterator();
	    while (itKey.hasNext()) {
	        K key = itKey.next();
	        if (bdic.isDefinedKeyAt(this.valueOf(key))) {
	            res.insert(key, bdic.valueOf(this.valueOf(key)));
            }
        }
	    return res;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
	    boolean res = true;
	    Iterator<K> it = bd.keys().iterator();
	    while (it.hasNext() && res) {
	        K key = it.next();
	        if (!bd.isDefinedValueAt(key)) {
	            res = false;
            }
        }
	    return res;
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		// TODO
		return null;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		// TODO
		return null;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
