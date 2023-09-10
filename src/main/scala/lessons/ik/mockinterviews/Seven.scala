package lessons.ik.mockinterviews

object Seven extends App {
  println("ABC".reverse)

  val sb = StringBuilder.newBuilder
}

/* Image Replication
 * =================
 *
 * A user's images are stored in a collection of databases within a data center.
 * The databases are physically constructed as a row of racks - e.g. a sample
 * collection of 4 racks would look like the following:
 *     A - B - C - D.
 * Whenever a user uploads an image, we save the image across multiple adjacent
 * databases. When we do this, we must replicate the image across the databases
 * adjacent to each other. For example, if we decide to replicate the image across
 * 2 databases, we can select from the following database sets: [A, B] [B, C] [C, D].
 * (A-C or B-D is not allowed because they are not adjacent).
 *
 * Each database has its own remaining space available. E.g.:
 * A(1MB) - B(5MB) - C(3MB) - D(2MB).
 * Because each set of databases have a differing remaining space available, and
 * we are replicating the image, the size of the image that can be saved will be
 * limited by the minimum space remaining across the selected database sets. For
 * example, if we are looking at the [A B] database set, the largest image that
 * can be saved is 1MB.
 *
 * Given:
 * int n: number of databases
 * array db: remaining space for each database
 * int x: number of replications
 *
 * Find the largest image that can be stored, that is, the maximum of the minimum
 * values of available space found while analyzing the databases in segments of x.
 *
 * Example 1:
 *
 * n = 4, the number of databases
 * db = [1, 5, 3, 2]
 * x = 2, the number of replications we want
 *
 * In this array of databases, the subarrays of size 2 are [1,5], [5,3], [3,2].
 * Thus, the initial analysis return 1, 3, 2, because those are the minima for
 * the segments. Finally, the maximum of these values is 3. Therefore, the answer
 * is 3.
 *
 * Example 2:
 *
 * n = 5
 * db = [2, 5, 4, 6, 8]
 * x = 3
 *
 * The subarrays of size x = 3 are [2, 5, 4], [5, 4, 6] and [4, 6,8]. The respective
 * minimum values for the three subarrays are 2, 4 and 4. The maximum of these values
 * is 4. Therefore, the answer is 4.
 *
 * Exmaple 3:
 *
 * n = 5
 * db = [1, 2, 3, 1, 2]
 * x = 1
 *
 * The answer is 3.
 *
 * Example 4:
 *
 * n = 3
 * db = [1, 1, 1]
 * x = 2
 *
 * The answer is 1.
 *
 * Tip: Please find a solution with a better time complexity than O(n^2).
 *
 * Constrains:
 * 1 <= n <= 10^6
 * 1 <= x <= n
 * 1 <= db[i] <= 10^9


 A = [2, 5, 4, 6, 8]
 x = 3 <====


 mono increasing q

 2, 4    min = 2
 4, 6    min = 4
 4, 6, 8 min = 4

 	public static int maxImageSize(int x, int[] db) {
		int max = db[0];
		int i = 0;
		// use a monotonically increasing queue to maintain
		// min (and min candidates) in a sliding window
		Deque<Integer> dq = new LinkedList<>();
		for (; i < x - 1; i++) {
			inQ(dq, db[i]);
		}
		for (; i < db.length; i++) {
			inQ(dq, db[i]);
			max = Math.max(max, dq.peekFirst());
			outQ(dq, db[i - x + 1]);
		}
		return max;
	}

	private static void inQ(Deque<Integer> dq, int item) {
		while (!dq.isEmpty() && dq.peekLast() > item) {
			dq.pollLast();
		}
		dq.offerLast(item);
	}

	private static void outQ(Deque<Integer> dq, int item) {
		if (dq.peekFirst() == item) {
			dq.pollFirst();
		}
	}


 values  = [2, 5, 4, 6, 8]
 indexes = [1, 2, 3, 4, 5]

 q = 2, 5, 4  min = 2

 q = 6 4 3   min

 q = 8 6 4

 j = 1
 j = 2
 j = 3
 winMin = -1

 if (j % 3 == 0) {
   winMin = winMin < q.top ? winMin : q.top
   globalMax = globalMax < winMin ? winMin : globalMax
   winMin = q.pop
   j = 1
 } else {
   winMin = winMin < q.top ? winMin : q.pop

   j += 1
 }
 -----

 if (A.isEmpty) return 0

 winMin = 1
 i = 1
 q.enqueue(A(0))
 max = 0

 while (!q.isEmpty) {
     if (i % x == 0) {
       // Global max
       winMin = winMin < q.pop ? winMin : q.pop
       max = max < winMin ? winMin : max
       winMin = q.top
     } else {
       // Window temp max
       winMin = winMin < q.pop ? winMin : q.pop

       // Prevent out of bounds
       if (i < A.length) q.enqueue(A(i-1))
     }
     i += 1
}

return max

 4 5 2


 */

