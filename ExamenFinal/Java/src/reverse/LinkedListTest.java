package reverse;

public class LinkedListTest {
    public static void main(String[] args) {
        LinkedList<Integer> list = LinkedList.testList();
        System.out.println("List before reverse");
        System.out.println(list);
        list.reverse();
        System.out.println("List after reverse");
        System.out.println(list);
    }
}
