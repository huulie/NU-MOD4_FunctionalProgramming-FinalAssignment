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
               bones[n-1] = new Bone(n, l, r);
                       n++;
           }
       }
   }

    /**
     * Returns the Double Six bone set as array of bones
     * @return array of bones
     */
   public Bone[] returnBones () {
       return bones;
   }
}
