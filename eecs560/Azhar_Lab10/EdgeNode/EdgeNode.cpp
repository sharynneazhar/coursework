/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class implementation for EdgeNode
 ============================================================================
*/

EdgeNode::EdgeNode() {}

EdgeNode::EdgeNode(const int weight, const int vert1, const int vert2) {
  w = weight;
  a = vert1;
  b = vert2;
}

int EdgeNode::getWeight() const {
  return w;
}

void EdgeNode::setWeight(const int weight) {
  w = weight;
}

int EdgeNode::getV1() const {
  return a;
}

void EdgeNode::setV1(const int vert1) {
  a = vert1;
}

int EdgeNode::getV2() const {
  return b;
}

void EdgeNode::setV2(const int vert2) {
  b = vert2;
}
