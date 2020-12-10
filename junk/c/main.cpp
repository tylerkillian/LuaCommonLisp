#include <cstdlib>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

void error(string errorMessage) {
  cerr << errorMessage << endl;
  exit(EXIT_FAILURE);
}

class LispExpression {
  public:

    LispExpression() {
    }

    ~LispExpression() {
    }

  private:
};

class LispCode {
  public:
    virtual string toString() = 0;
};

class LispSymbol : public LispCode {
  public:
    LispSymbol(string value) {
      this->value = value;
    }

    virtual string toString() {
      return this->value;
    }

  private:
    string value;
};

class LispList : public LispCode {
  public:
    LispList() {
    }

    void push(LispCode *code) {
      elements.push_back(code);
    }

    virtual string toString() {
      if (elements.size() == 0) {
        return "()";
      }

      string result = "";
      for (vector<LispCode*>::iterator current = elements.begin(); current != elements.end(); current++) {
        result += " " + (*current)->toString();
      }
      return "(" + result.substr(1) + ")";
    }
  private:
    vector<LispCode*> elements;
};

class LispExpressionReader {
  public:

    LispExpressionReader(string filename) {
      input.open(filename.c_str());

      if (!input.is_open()) {
        error("Could not open " + filename);
      }
removeThisVariable = 0;
    }

    ~LispExpressionReader() {
      input.close();
    }

    LispExpression* getNextExpression() {
if (removeThisVariable == 0) {
  removeThisVariable++;
  return new LispExpression;
}
      return NULL;
    }

  private:

    ifstream input;
int removeThisVariable;
};

int main(int argc, char **argv)
{
  if (argc == 1) {
    error("Usage: " + string(argv[0]) + " <filename>");
  }
  string inputFilename(argv[1]);

  LispExpressionReader expressionReader(inputFilename);
  LispExpression *expression = expressionReader.getNextExpression();
  while (expression) {
cout << "inside loop" << endl;
    //environment.evalute(expression);

    delete expression;

    expression = expressionReader.getNextExpression();
  }

  LispList *testList = new LispList();
  testList->push(new LispSymbol("setf"));
  testList->push(new LispSymbol("a"));
  testList->push(new LispSymbol("2"));
  cout << "Expression == " << testList->toString() << endl;

  return 0;
}
