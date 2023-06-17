import { Constraint, Method } from "../types";
import { Box, Text } from "@chakra-ui/react";
import { prettyPrintMethod, stringToColor } from "../util";

interface Props {
  constraint: Constraint;
  highlighted: string[];
  isHighlighted: boolean;
  highlightColor: string;
}

const ConstraintDisplay = ({
  constraint,
  highlighted,
  isHighlighted,
  highlightColor,
}: Props) => (
  <Box
    padding={isHighlighted ? "3px" : "6px"}
    borderRadius="5"
    borderWidth={isHighlighted ? 3 : 0}
    borderColor={highlightColor}
  >
    <Text as="b">Constraint {constraint.constraintName}:</Text>
    {constraint.methods.map((method: Method) => (
      <Box
        key={method.methodName}
        padding={highlighted.includes(method.methodName) ? "3px" : "6px"}
        borderRadius="5"
        borderWidth={highlighted.includes(method.methodName) ? 3 : 0}
        borderColor={stringToColor(method.methodName)}
      >
        <Text key={method.methodName}>
          {`${method.methodName}: ${prettyPrintMethod(method)}`}
        </Text>
      </Box>
    ))}
  </Box>
);

export default ConstraintDisplay;
