import { IComponent } from "../types";
import { Text, Card, CardHeader, CardBody, Wrap } from "@chakra-ui/react";
import VariableDisplay from "./VariableDisplay";
import { stringToColor } from "../util";

interface Props extends IComponent {
  highlighted: string[];
}

const Component = ({
  compID: identifier,
  variables,
  strength,
  highlighted,
}: Props) => (
  <Card maxW="sm" bg="gray.800" borderRadius="lg" overflow="hidden">
    <CardHeader>
      <Text>Component {identifier}</Text>
    </CardHeader>
    <CardBody>
      <Wrap>
        {variables
          .slice()
          .reverse()
          .map((variable) => (
            <VariableDisplay
              key={variable.varID}
              variable={variable}
              isHighlighted={highlighted.includes(variable.varID)}
              highlightColor={stringToColor(variable.varID)}
            />
          ))}
      </Wrap>
    </CardBody>
  </Card>
);

export default Component;
