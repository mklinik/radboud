
/* ESC/Java2 Exercise: 
   
   This class implements a Bag of integers, using an array.

   Add JML specs to this class, to stop ESC/Java2 from complaining.

   NB there may be errors in the code that you have to fix to stop 
   ESC/Java2 from complaining, as these complaints of ESC/Java2 
   point to real bugs in the code. But keep changes to the code to
   a minimum of what is strictly needed. 
   Mark any changes you have made in the code with a comment,
   eg to indicate that you replaced <= by <.


   The only JML keywords needed for this are
      requires
      invariant 
      ensures 
  
   If you want, you can also use
      non_null
   as abbreviation.


   While developing your specs, it may be useful to use the keywords
      assert
   to add additional assertions in source code, to find out what 
   ESC/Java2 can - or cannot - prove at a given program point.
  
*/

class Bag {

  //@ non_null
  int[] contents;
  //@ invariant n <= contents.length;
  //@ invariant n >= 0;
  int n;

  //@ requires input != null;
  Bag(int[] input) {
    n = input.length;
    contents = new int[n];
    arraycopy(input, 0, contents, 0, n);
  }

  Bag() {
    n =0;
    contents = new int[0];
  }

  void removeOnce(int elt) {
    // replaced <= by <
    for (int i = 0; i < n; i++) {  
      if (contents[i] == elt ) {
         n--;
         contents[i] = contents[n];
         return;
      }
    }
  }

  void removeAll(int elt) {
    // replaced <= by <
    for (int i = 0; i < n; i++) {   
      if (contents[i] == elt ) {
         n--;
         contents[i] = contents[n];
      }
    }
  }

  int getCount(int elt) {
    int count = 0;
    // replaced <= by <
    for (int i = 0; i < n; i++) 
      if (contents[i] == elt) count++; 
    return count;
  }

  /* Warning: you may have a hard time checking the method "add" below.
     ESC/Java2 may warn about a very subtle bug that can be hard to spot. 
     If you cannot find the problem after staring at the code for an hour, 
     feel free to stop.
   */

  void add(int elt) {
    if (n == contents.length) {
        // if n == 0 then this doesn't increase in size
        // changed 2*n to 2*n+1
       int[] new_contents = new int[2*n+1]; 
       arraycopy(contents, 0, new_contents, 0, n);
       contents = new_contents;
    }
    contents[n]=elt;
    n++;
  }

  //@ requires b != null;
  void add(Bag b) {
    int[] new_contents = new int[n + b.n];
    arraycopy(contents, 0, new_contents, 0, n);
    // elements of this bag will occupy indexes 0 to n-1
    // elements of b will occupy indexes n to n+b.n
    // changed destOff parameter from n+1 to n
    arraycopy(b.contents, 0, new_contents, n, b.n);
    contents = new_contents; 
  }

  //@ requires a != null;
  void add(int[] a) {
    this.add(new Bag(a));
  }

  //@ requires b != null;
  Bag(Bag b) {
    // explicitly call standard constructor to establish object invariants
    this();
    this.add(b);    
  }


  //@ requires src != null;
  //@ requires dest != null;
  //@ requires srcOff >= 0;
  //@ requires destOff >= 0;
  //@ requires length >= 0;
  //@ requires dest.length >= destOff + length;
  //@ requires src.length >= srcOff + length;
  private static void arraycopy(int[] src,
                                int   srcOff,
                                int[] dest,
                                int   destOff,
                                int   length) {
    for( int i=0 ; i<length; i++) {
       dest[destOff+i] = src[srcOff+i];
    }
  }
  
}
