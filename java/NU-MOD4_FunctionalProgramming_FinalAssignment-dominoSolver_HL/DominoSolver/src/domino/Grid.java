package domino;

public class Grid<T> {

    public final static int WIDTH = 8;
    public final static int HEIGHT = 7;

    private final T[][] elements;

    protected Grid(T[][] emptyGrid) {
        this.elements = emptyGrid;
    }

    public boolean isOnBoard (Position position) {
        return position.getX() >= 0 && position.getX() < WIDTH && position.getY() >= 0 && position.getY() < HEIGHT;
    }

    public boolean isFree(Position position) {
        return this.getElementAt(position) == null;
    }

    public T getElementAt(Position position) {
        return this.elements[position.getX()][position.getY()];
    }

    public void setElementAt(T element, Position position) {
        this.elements[position.getX()][position.getY()] = element;
    }

    /**
     * Returns next empty position on the board, when traversing it in row-major order
     * @param position as reference
     * @return next empty Position, in row-major order
     */
    public Position nextEmptyPosition(Position position) {
        if(position.getX() == -1 && position.getY() == -1) {
            return new Position(0,0); // starting position
        }

        Position currentPosition = position;

        while (this.getElementAt(currentPosition) != null) {
            currentPosition = Position.next(currentPosition);
            if(!isOnBoard(currentPosition)) {
                return null; // should never happen, because Position.next() starts at origin after last Position
            }
        }
        return currentPosition;
    }

    /**
     * Position on the grid
     */
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

        /**
         * Returns next position on the board, when traversing it in row-major order
         * @param position as reference
         * @return next Position in row-major order
         */
        public static Position next(Position position) {
            if (position.getX() == WIDTH-1 && position.getY() == HEIGHT-1) {
                return new Position(0,0);
            } else {
                return index2position(position2index(position) + 1);
            }
        }

        /**
         * Returns second position, that is horizontal to the first position
         * @param position as reference
         * @return position horizontal from reference
         */
        public static Position horizontal(Position position) {
            return new Position(position.getX()+1, position.getY());
        }

        /**
         * Returns second position, that is vertical to the first position
         * @param position as reference
         * @return position vertical from reference
         */
        public static Position vertical(Position position) {
            return new Position(position.getX(), position.getY()+1);
        }
    }
}
