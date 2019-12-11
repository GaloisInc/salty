import demo.Democontroller1;
import demo.enums.Behavior;

class DemoTest {
    public static void main(String[] args) {
        Democontroller1 cont = new Democontroller1();

        System.out.println(cont.move(true,true,true));
        System.out.println(cont.move(true,true,false));
    }
}
