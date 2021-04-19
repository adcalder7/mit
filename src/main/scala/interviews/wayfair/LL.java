package interviews.wayfair;

public class LL {

    static int count = 0;
    public static void main(String[] args) {
        if (count < 3) {
            count++;
            main(null);
        } else {
            return;
        }
        System.out.println("Hello World!");


    }

}
