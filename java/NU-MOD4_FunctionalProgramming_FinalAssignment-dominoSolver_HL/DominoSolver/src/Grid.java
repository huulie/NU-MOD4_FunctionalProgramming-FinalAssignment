public class Grid {

    protected final static int WIDTH = 8; // TODO make configurable? modifiers
    protected final static int HEIGHT = 7; // TODO make configurable? modifiers

    private Object[][] elements;

   public boolean isOnBoard (Position position) {
       return position.getX() >= 0 && position.getX() < this.WIDTH && position.getY() >= 0 && position.getY() < this.HEIGHT;
   }

   public boolean isFree(Position position) {
       return this.getElement(position) == null;
   }

   private Object getElement (Position position) {
       return elements[position.getX()][position.getY()];
   }

   private void setElement (Object element, Position position) {
       elements[position.getX()][position.getY()] = element;
   }

//   private grid DeepCopy TODO

   public static class Position {
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

       public static int position2index(Position position) {
          return position.getY() * WIDTH + position.getX();
       }

       public static Position index2position(int index) {
           return new Position(index % WIDTH, index / WIDTH);
       }

       public static Position next(Position position) { // TODO modifier
           if (position.getX() == WIDTH && position.getY() == HEIGHT) {
               return new Position(0,0);
           } else {
               return index2position(position2index(position) + 1);
           }
       }

       // TODO keep orientations here, and inverse?
       public static Position horizontal(Position position) {
           return new Position(position.getX()+1, position.getY());
       }
       public static Position vertical(Position position) {
           return new Position(position.getX(), position.getY()+1);
       }
   }
}
