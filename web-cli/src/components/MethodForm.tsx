import {
  Text,
  InputGroup,
  Input,
  Checkbox,
  Flex,
  VStack,
} from "@chakra-ui/react";
import { Method, Variable } from "../types";

interface Props {
  variables: Variable[];
  method: Method;
  setMethods: React.Dispatch<React.SetStateAction<Method[]>>;
}

const MethodForm = ({ variables, method, setMethods }: Props) => {
  return (
    <>
      <Text>Method name</Text>
      <InputGroup mb={5}>
        <Input
          type="text"
          placeholder="Method name"
          value={method.methodName}
          onChange={(e) => {
            setMethods((prevMethods) =>
              prevMethods.map((prevMethod) => {
                if (prevMethod.methodName === method.methodName) {
                  return {
                    ...prevMethod,
                    methodName: e.target.value,
                  };
                }
                return prevMethod;
              })
            );
          }}
        />
      </InputGroup>
      <Text>Input variables</Text>
      <VStack mb="5" align="left">
        {variables.map((variable) => (
          <Flex key={variable.varID}>
            <Checkbox
              mr={2}
              value={variable.varID}
              onChange={(e) => {
                setMethods((prevMethods) =>
                  prevMethods.map((prevMet) => {
                    if (prevMet.methodName === method.methodName) {
                      if (e.target.checked) {
                        return {
                          ...prevMet,
                          inputs: [...prevMet.inputs, e.target.value],
                        };
                      } else {
                        return {
                          ...prevMet,
                          inputs: prevMet.inputs.filter(
                            (input) => input !== e.target.value
                          ),
                        };
                      }
                    } else {
                      return prevMet;
                    }
                  })
                );
              }}
            />
            <Text>{variable.varID}</Text>
          </Flex>
        ))}
      </VStack>
      <Text>
        Expressions for output variables (leave blank for no expression)
      </Text>
      {method.expressions.map((expression) => (
        <InputGroup key={expression[0]} mb={5}>
          <Input
            type="text"
            placeholder={`Expression for ${expression[0]}`}
            value={expression[1]}
            onChange={(e) => {
              setMethods((prevMethods) =>
                prevMethods.map((prevMethod) => {
                  if (prevMethod.methodName === method.methodName) {
                    return {
                      ...prevMethod,
                      expressions: prevMethod.expressions.map(
                        (prevExpression) =>
                          prevExpression[0] === expression[0]
                            ? [prevExpression[0], e.target.value]
                            : prevExpression
                      ),
                    };
                  }
                  return prevMethod;
                })
              );
            }}
          />
        </InputGroup>
      ))}
    </>
  );
};

export default MethodForm;
