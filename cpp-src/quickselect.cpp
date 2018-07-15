#include <vector>
#include <iostream>

using namespace std;

bool comes_before(int x, int y) {
  return (x < y);
}

void check_index_ordering_invariant(int lo, int hi, int len) {
  if (!(1 <= lo && lo <= hi + 1 && hi + 1 <= len)) {
    cout << "check_index_ordering_invariant violated: "
      << lo << ", " << hi << ", " << len << endl;
  }
}

void check_partition_condition_invariant(int lo, int hi, vector<int> xs) {
  int pivot = xs[0];
  for (int i = 1; i < lo; i++) {
    if (comes_before(pivot, xs[i])) {
      cout << "check_partition_condition_invariant violated on smalls: "
          << lo << ", "
          << hi << ", "
          << "[";
      for (auto x : xs) {
          cout << x << ", ";
      }
      cout << "]" << endl;
      return;
    }
  }
  for (int i = hi + 1; i < xs.size(); i++) {
    if (comes_before(xs[i], pivot)) {
      cout << "check_partition_condition_invariant violated on larges: "
          << "lo: " << lo << ", "
          << "hi: " << hi << ", "
          << "i: " << i << ", "
          << "xs: " << "[";
      for (auto x : xs) {
        cout << x << ", ";
      }
      cout << "]" << endl;
      return;
    }
  }
}

int partition(vector<int> &xs) {
  if (xs.size() == 0) {
    return -1;
  } else if (xs.size() == 1) {
    return 0;
  } else {
    int pivot = xs[0];
    int lo = 1;
    int hi = xs.size() - 1;
    check_index_ordering_invariant(lo, hi, xs.size());
    while (lo < hi) {
      if (comes_before(xs[lo], pivot)) {
        lo++;
        if (!comes_before(xs[hi], pivot)) {
          hi--;
        }
      } else if (!(comes_before(xs[hi], pivot))) {
        hi--;
      } else {
        int temp = xs[lo];
        xs[lo] = xs[hi];
        xs[hi] = temp;
        lo++;
        hi--;
      }
      check_partition_condition_invariant(lo, hi, xs);
      check_index_ordering_invariant(lo, hi, xs.size());
    }
    int pivot_i = lo - 1;
    int temp = xs[0];
    xs[0] = xs[pivot_i];
    xs[pivot_i] = temp;
    return pivot_i;
  }
}

int main(int argc, char * argv[]) {
  vector<int> xs {-11,29,-10,-16,20,19,-13,-8,13,-12,-30,19,3,4,28,-7,7,11,-24,5,9,0};

  cout << "[";
  for (auto x : xs) {
    cout << x << ", ";
  }
  cout << "]" << endl;

  int pivot_i = partition(xs);
  cout << "Pivot index: " << pivot_i << endl;

  cout << "[";
  for (auto x : xs) {
    cout << x << ", ";
  }
  cout << "]" << endl;

  return 0;
}
