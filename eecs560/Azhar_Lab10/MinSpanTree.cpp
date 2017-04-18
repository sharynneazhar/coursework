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

void MinSpanTree::runKruskal() {
  std::cout << "\nKruskal: ";

  int *arr = new int[dim];
  for (int i = 0; i < dim; i++) {
    arr[i] = i;
  }

  C = new DisjointSet<int>(arr, dim);
  delete [] arr;
  arr = nullptr;

  // get the number of edges
  int numEdges = 0;
  for (int i = 0; i < dim; i++) {
    for (int j = i; j < dim; j++) {
      if (adjMatrix[i][j] != 0) {
        numEdges++;
      }
    }
  }

  // initialize a priority queue to keep track of edges in length order
  edgesQueue = new EdgeNode*[numEdges];

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
  sort(edgesQueue, numEdges);

  EdgeNode **E = new EdgeNode*[dim - 1];
  for (int i = 0; i < dim - 1; i++) {
    E[i] = nullptr;
  }

  EdgeNode *x;
  int cardinality = 0;
  int iter = 0;
  while (!(emptyQueue(edgesQueue, numEdges)) && cardinality != dim - 1) {
    x = edgesQueue[iter];
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
      std::cout << "No solution" << std::endl;
      return;
    }
  }

  for (int i = 0; i < dim - 1; i++) {
    std::cout << "(" << E[i]->getV1() << ", " << E[i]->getV2() << ") ";
  }

  // free memory
  delete [] E;
  delete [] edgesQueue;
  delete C;
  E = nullptr;
  edgesQueue = nullptr;
  C = nullptr;
}

void MinSpanTree::runPrim() {
  std::cout << "\nPrim: ";

  EdgeNode **E = new EdgeNode *[dim - 1];
  for (int i = 0; i < dim - 1; i++) {
    E[i] = nullptr;
  }

  int *V = new int[dim];
  for (int i = 0; i < dim; i++) {
    V[i] = -1;
  }

  V[0] = 0;

  int squareDim = dim * dim;
  edgesQueue = new EdgeNode*[squareDim];

  int idx = 0;
  for (int i = 1; i < dim; i++) {
    if (adjMatrix[0][i] != 0) {
      edgesQueue[idx] = new EdgeNode(adjMatrix[0][i], 0, i);
      idx++;
    }
  }

  sort(edgesQueue, squareDim);

  EdgeNode *x;
  int cardinality = 0;
  while (!(emptyQueue(edgesQueue, squareDim)) && cardinality != dim) {
    x = dequeue(edgesQueue, squareDim);
    E[cardinality] = x;
    V[cardinality + 1] = x->getV2();
    cardinality++;
    updateQueue(edgesQueue, V);
  }

  for (int i = 0; i < dim - 1; i++) {
    if (E[i] == nullptr) {
      std::cout << "No solution" << std::endl;
      return;
    }
  }

  for (int i = 0; i < dim - 1; i++) {
    std::cout << "(" << E[i]->getV1() << ", " << E[i]->getV2() << ") ";
  }

  // free memory
  delete [] E;
  delete [] V;
  delete [] edgesQueue;
  E = nullptr;
  V = nullptr;
  edgesQueue = nullptr;
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

bool MinSpanTree::element(int elem, int *V) {
  for (int i = 0; i < dim; i++) {
    if (V[i] == elem) {
      return true;
    }
  }
  return false;
}

EdgeNode *MinSpanTree::dequeue(EdgeNode **queue, int size) {
  for (int i = 0; i < size; i++) {
    if (queue[i] != nullptr) {
      return queue[i];
    }
  }
  return nullptr;
}

void MinSpanTree::updateQueue(EdgeNode **queue, int *V) {
  for (int i = 0; i < dim * dim; i++) {
    queue[i] = nullptr;
  }

  int k = 0;
  for (int i = 0; i < dim; i++) {
    for (int j = 0; j < dim; j++) {
      if (adjMatrix[i][j] != 0 && element(i, V) && !(element(j, V))) {
        queue[k] = new EdgeNode(adjMatrix[i][j], i, j);
        k++;
      }
    }
  }
  sort(queue, dim * dim);
}

bool MinSpanTree::emptyQueue(EdgeNode **queue, int size) {
  for (int i = 0; i < size; i++) {
    if (queue[i] != nullptr) {
      return false;
    }
  }
  return true;
}
