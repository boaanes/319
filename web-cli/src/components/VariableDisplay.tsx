import { VStack, Text, Tag } from "@chakra-ui/react";
import { Variable } from "../types";

interface Props {
  variable: Variable;
  isHighlighted: boolean;
  highlightColor: string;
}

const VariableDisplay = ({
  variable,
  isHighlighted,
  highlightColor,
}: Props) => (
  <VStack
    padding={isHighlighted ? "3px" : "6px"}
    borderRadius="5"
    borderWidth={isHighlighted ? 3 : 0}
    borderColor={highlightColor}
  >
    <Tag>{variable.varValue.toString()}</Tag>
    <Text>{variable.varID}</Text>
  </VStack>
);

export default VariableDisplay;
