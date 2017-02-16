/**
*	@file : HashObject.h
*	@author : Sharynne Azhar
*	@date : 02-11-2017
*/

#ifndef HASH_OBJECT_H
#define HASH_OBJECT_H

template <typename T>
class HashObject {
  private:
    T m_value;
    bool m_flag;

  public:
    HashObject() {
      m_value = -1;
      m_flag = false;
    }

    HashObject(T value) {
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
