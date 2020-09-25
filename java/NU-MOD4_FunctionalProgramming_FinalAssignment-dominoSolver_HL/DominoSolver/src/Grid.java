public class Grid {

    protected final int WIDTH = 8; // TODO make configurable?
    protected final int HEIGHT = 7; // TODO make configurable?

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

       public int position2index(Position position) {
          return position.getY() * WIDTH + position.getX();
       }

       public Position index2position(int index) {
           return new Position(index % WIDTH, index / WIDTH);
       }

       private Position next (Position position) {
           if (position.getX() == WIDTH && position.getY() == HEIGHT) {
               return new Position(0,0);
           } else {
               return index2position(position2index(position) + 1);
           }
       }

       // TODO keep orientations here, and inverse?
       private Position horizontal(Position position) {
           return new Position(position.getX()+1, position.getY());
       }
       private Position vertical(Position position) {
           return new Position(position.getX(), position.getY()+1);
       }
   }
}
