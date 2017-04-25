/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class implementation for MinSpanTree
 ============================================================================
*/

MinSpanTree::MinSpanTree() {}

MinSpanTree::MinSpanTree(int **matrix, int dimension) {
  adjMatrix = matrix;
  dim = dimension;
}

MinSpanTree::~MinSpanTree() {
  for (int i = 0; i < dim; i++) {
    delete [] adjMatrix[i];
  }
  delete [] adjMatrix;
}

EdgeNode** MinSpanTree::runKruskal() {
  // initialize the candidate set
  C = new DisjointSet(dim);

  // initialize a priority queue to keep track of edges in length order
  edgesQueue = new EdgeNode*[dim * dim];

  // fill priority queue with the edges
  int idx = 0;
  for (int i = 0; i < dim; i++) {
    for (int j = i; j < dim; j++) {
      if (adjMatrix[i][j] != 0) {
        edgesQueue[idx] = new EdgeNode(adjMatrix[i][j], i, j);
        idx++;
      }
    }
  }

  // sort the edges from shortest to longest
  sort(edgesQueue, dim);

  EdgeNode **E = new EdgeNode*[dim - 1];
  for (int i = 0; i < dim - 1; i++) {
    E[i] = nullptr;
  }

  int cardinality = 0;
  int iter = 0;
  while (!(emptyQueue(edgesQueue, dim)) && cardinality != dim - 1) {
    EdgeNode *x = edgesQueue[iter];
    edgesQueue[iter] = nullptr;
    if (C->find(x->getV1()) != C->find(x->getV2())) {
      E[cardinality] = x;
      C->setUnion(x->getV1(), x->getV2());
      cardinality++;
    }
    iter++;
  }

  for (int i = 0; i < dim - 1; i++) {
    if (E[i] == nullptr) {
      return nullptr;
    }
  }

  // free memory
  delete [] E;
  delete [] edgesQueue;
  delete C;
  E = nullptr;
  edgesQueue = nullptr;
  C = nullptr;

  return E;
}

void MinSpanTree::runPrim() {
  int parent[dim];
  int Q[dim];
  bool selected[dim];

  // initialize the values
  for (int i = 0; i < dim; i++) {
    Q[i] = -1;
    selected[i] = false;
  }

  // select an arbitrary (first) vertex to grow MST
  Q[0] = 0;
  parent[0] = -1;

  // go through the vertices
  for (int i = 0; i < dim - 1; i++) {
    // select the min cost edge from the set of vertices
    int j = minKey(Q, selected);

    // update the queue
    selected[j] = true;

    // update Q
    for (int j = 0; j < dim; j++) {
      if (adjMatrix[j][j] && selected[j] == false && adjMatrix[j][j] < Q[j]) {
        parent[j] = j;
        Q[j] = adjMatrix[j][j];
      }
    }
  }
}

void MinSpanTree::sort(EdgeNode **queue, int size) {
  EdgeNode *swap = nullptr;
  for (int i = 0; i < size - 1; i++) {
    for (int j = 0; j < size - i - 1; j++) {
      if (queue[j + 1] == nullptr || queue[j]->getWeight() > queue[j + 1]->getWeight()) {
        swap = queue[j];
        queue[j] = queue[j + 1];
        queue[j + 1] = swap;
      }
    }
  }
}

bool MinSpanTree::emptyQueue(EdgeNode **queue, int size) {
  for (int i = 0; i < size; i++) {
    if (queue[i] != nullptr) {
      return false;
    }
  }
  return true;
}

int MinSpanTree::minKey(int values[], bool selected[]) {
  int min, min_index;
  for (int v = 0; v < dim; v++) {
    if (selected[v] == false && values[v] < min) {
      min = values[v];
      min_index = v;
    }
  }
  return min_index;
}
