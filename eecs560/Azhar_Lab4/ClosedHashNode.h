/**
*	@file   : ClosedHashNode.h
*	@author : Sharynne Azhar
*	@date   : 02-17-2017
* @desc   : Holds the node object for closed hashing
*/

#ifndef CLOSED_HASH_NODE_H
#define CLOSED_HASH_NODE_H

template <typename T>
class ClosedHashNode {
  private:
    T m_value;
    bool m_flag;

  public:
    ClosedHashNode() {
      m_value = -1;
      m_flag = false;
    }

    ClosedHashNode(T value) {
      m_value = value;
      m_flag = false;
    }

    T getValue() const {
      return m_value;
    }

    void setValue(const T val) {
      m_value = val;
    }

    bool getFlag() const {
      return m_flag;
    }

    void setFlag(const bool value) {
      m_flag = value;
    }

    bool insertable() {
      return ((m_value == -1) && (!m_flag));
    }
};

#endif
