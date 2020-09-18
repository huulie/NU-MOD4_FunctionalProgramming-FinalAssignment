public class BoneSet {

   Bone[] bones;

    /**
     * Generates a Double Six bone set
     */
   public BoneSet() {
       bones = new Bone[28];

       int n = 1;
       for (int l = 0; l < 7; l++) {
           for (int r = l ; r < 7; r++) {
               bones[n] = new Bone(n, l, r);
                       n++;
           }
       }

   }

    public static void main(String[] args) {
        BoneSet test = new BoneSet();
        System.out.println(test);
    }
}
