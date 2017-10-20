/**
 * Assignment 1 - NFA to DFA Converter
 * @author  Sharynne Azhar
 * @date    10-13-2016
 * @file    Converter.cpp
 * @desc    Converts an input NFA into its equivalent DFA
 */

Converter::Converter(std::vector<std::string> vec) : inputVector(vec) {}

Converter::~Converter() {}

void Converter::setup() {
  // Get the initial state from the first line
  initialState = std::stoi(utils.findString(inputVector[0], "{", "}"));
  // Get the final states from the second line
  std::string finalStatesStr = utils.findString(inputVector[1], "{", "}");
  finalStates = utils.stoiVec(utils.splitString(finalStatesStr, ","));

  // Get the total number of states from the third line
  numStates = std::stoi(utils.findString(inputVector[2], ":"));

  // Get the state letters from the fourth line
  for (char terminal : inputVector[3].substr(5, inputVector[3].length())) {
    if (!isspace(terminal)) {
      stateSymbols.push_back(terminal);
      numStateSymbols++;
    }
  }

  // Get the state transitions
  // TODO: figure out how to refactor this to follow the cleaner findString method used above
  bool endBracket = false;
  std::string temp = "";
  for (int i = 0; i < numStates ; i++){
    inputVector[i + 4].erase(std::remove_if(inputVector[i + 4].begin(), inputVector[i+4].end(), isspace), inputVector[i + 4].end());
    while (inputVector[i + 4][0] != '{') {
      inputVector[i + 4].erase(inputVector[i + 4].begin(),inputVector[i + 4].begin() + 1);
    }
    inputVector[i + 4].erase(std::remove(inputVector[i + 4].begin(),inputVector[i + 4].end(), '{'), inputVector[i + 4].end());
    for (int j = 0; j <= numStateSymbols - 1; j++) {
      endBracket = false; temp.clear();
      while (!endBracket) {
        if (inputVector[i + 4][0] != '}') {
          temp += inputVector[i + 4][0];
          inputVector[i + 4].erase(inputVector[i + 4].begin(),inputVector[i + 4].begin() + 1);
        } else {
          temp += inputVector[i + 4][0];
          inputVector[i + 4].erase(inputVector[i + 4].begin(),inputVector[i + 4].begin() + 1);
          endBracket = true;
          temp.erase(temp.end() - 1,temp.end());
          nextTransitions.push_back(temp);
        }
      }
    }
  }

  load();
}

// Load the transitions
void Converter::load() {
  int index = 0;
  for (int i = 0; i < numStates; i++) {
    for (int j = 0; j < numStateSymbols; j++) {
      // If the state symbol isn't an epsilon
      if (j != numStateSymbols - 1) {
        // if the next transition state is empty, use 0 as placeholder
        std::string str = (nextTransitions[index] == "") ?  "0" : nextTransitions[index];
        intNextTransitions.push_back(std::stoi(str));
      } else {
        std::vector<int> vec;
        nextTransitions[index].push_back(','); // filler
        while (nextTransitions[index].size() != 0) {
          bool end = false;
          std::string tempString = "";
          while (!end) {
            if (nextTransitions[index][0] != ',') {
              tempString += nextTransitions[index][0];
              nextTransitions[index].erase(nextTransitions[index].begin(), nextTransitions[index].begin() + 1);
            } else {
              int tempInt = 0;
              nextTransitions[index].erase(nextTransitions[index].begin(), nextTransitions[index].begin() + 1);
              std::stringstream stream(tempString);
              stream >> tempInt;
              vec.push_back(tempInt);
              end = true;
            }

            if (nextTransitions[index].size() == 0)
              end = true;
          }
        }

        epsilonStates.push_back(vec);
      }

      index++;
    }
  }
}

std::vector<int> Converter::epsilonClosure(std::vector<int> &statesVec, int markNum) {
  int markIndex = statesVec[markNum] - 1;
  for (std::size_t i = markNum; i < statesVec.size(); i++) {
    if (epsilonStates[markIndex][i] != 0) {
      for (std::size_t j = 0; j < epsilonStates[markIndex].size(); j++) {
        if (epsilonStates[markIndex][j] != 0) {
          bool close = true;
          for (std::size_t i = 0; i < statesVec.size(); i++) {
            if (epsilonStates[markIndex][j] == statesVec[i])
              close = false;
          }

          if (close) {
            statesVec.push_back(epsilonStates[markIndex][j]);
            epsilonClosure(statesVec, ++i);
          }
        }
      }
    }
  }

  return statesVec;
}

void Converter::mark(std::vector<int> statesVec,int marker) {
  std::vector<int> startState;
  std::vector<int> curStates;

  int index;
  bool endState;
  bool markedAll;

  for (std::size_t i = 0; i < statesVec.size(); i++) {
    startState.push_back(statesVec[i]);
  }

  std::cout << "\nMark " << marker << std::endl;
  for (int j = 0; j < numStateSymbols - 1; j++) {
    markedAll = true;
    for (std::size_t i = 0; i < statesVec.size(); i++) {
      index = (statesVec[i] == 1) ? 0 : (statesVec[i] * (numStateSymbols - 1)) - (numStateSymbols - 1);
      index += j;
      if (intNextTransitions[index] != 0) {
        curStates.push_back(intNextTransitions[index]);
        markedAll = false;
      }
    }

    if (markedAll)
      curStates.push_back(0);
  }

  for (int i = 0; i < numStateSymbols -1; i++) {
    if (curStates[i] < (numStates * (numStateSymbols - 1) ) && curStates[i] != 0) {
      endState = true;
      utils.printVec(statesVec);
      std::cout << " -- " << stateSymbols[i] << " --> {";
      std::cout << curStates[i] << "}" << std::endl;

      std::vector<int> vec;
      std::cout << "E-Closure{" << curStates[i] << "} = ";
      vec.push_back(curStates[i]);
      vec = epsilonClosure(vec, 0);
      utils.printVec(vec);

      dfaStates.push_back(vec);
      dfaTransitionNum.push_back(dfaStates.size());
      std::cout << " = " <<  dfaStates.size() << std::endl;

      if (vec.size() == startState.size()) {
        for (std::size_t j = 0; j < vec.size(); j++) {
          if (vec[j] != startState[j])
            endState = false;
        }
      } else
        endState = false;
    } else {
      dfaTransitionNum.push_back(0);
    }

  }

  if (!endState) {
    marker++;
    std::cout << std::endl;
    mark(dfaStates[marker - 1], marker);
  }

  if (endState)
    dfaFinalStates = marker;
}

void Converter::convert() {
  std::vector<int> stateInProcess;
  stateInProcess.push_back(initialState);
  stateInProcess = epsilonClosure(stateInProcess, 0);
  dfaStates.push_back(stateInProcess);
  std::cout << "\nE-closure(IO) = ";
  utils.printVec(dfaStates[0]);
  std::cout << " = 1\n";
  mark(dfaStates[0], 1);
}

void Converter::getDFAInfo() {
  bool includeInFinalDFA;
  for (std::size_t i = 0; i < dfaStates.size(); i++) {
    for (std::size_t j = 0; j < dfaStates[i].size(); j++) {
      if (initialState == dfaStates[i][j])
        initialState = i + 1;
      for (std::size_t k = 0; k < dfaStates[dfaFinalStates].size(); k++){
        if (dfaStates[i].size() == dfaStates[dfaFinalStates].size()) {
          if (dfaStates[i][j] == dfaStates[dfaFinalStates][k])
            includeInFinalDFA = true;
          for (std::size_t q = 0; q < dfaFinalVector.size(); q++) {
            if (i - 1 == dfaFinalVector[q])
              includeInFinalDFA = false;
            else
              includeInFinalDFA = true;
          }

          if (includeInFinalDFA)
            dfaFinalVector.push_back(i - 1);
        }
      }
    }
  }

  std::cout << "\nInitial State: {" << initialState << "}" << std::endl;
  std::cout << "Final States: "; utils.printVec(dfaFinalVector);
  std::cout << std::endl;
}

void Converter::printResults() {
  int index;
  getDFAInfo();

  std::cout << "State    ";
  for (int i = 0; i < numStateSymbols -1; i++) {
    std::cout << stateSymbols[i] << "     ";
  }

  std::cout << std::endl;

  for (std::size_t i = 0; i < dfaStates.size(); i++) {
    std::cout << i + 1 << "       {";

    for (int j = 0; j < numStateSymbols -1; j++) {
      index = (i * (numStateSymbols -1)) + j;

      if (dfaTransitionNum[index] == 0) {
       std::cout << " }";
      } else {
       std::cout << dfaTransitionNum[index] << "}";
      }

      if (j != numStateSymbols - 2) {
        std::cout << "   {";
      } else{
        std::cout << std::endl;
      }
    }
  }
}
