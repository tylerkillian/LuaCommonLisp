#include <cstdlib>
#include <fstream>
#include <iostream>
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
  if (argc == 0) {
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

  return 0;
}
