public class Grid {

    private int WIDTH = 8; // TODO make configurable?
    private int HEIGHT = 7; // TODO make configurable?

   public boolean isOnBoard (Position position) {
       return position.getX() >= 0 && position.getX() < this.WIDTH && position.getY() >= 0 && position.getY() < this.WIDTH;
   }

   public class Position {
       /**
        * Column
        */
       private int x;

       /**
        * Row
        */
       private int y;

       public Position (int x, int y) {
           this.setX(x);
           this.setY(y);
       }

       public int getX() {
           return x;
       }

       public void setX(int x) {
           this.x = x;
       }

       public int getY() {
           return y;
       }

       public void setY(int y) {
           this.y = y;
       }




   }
}
