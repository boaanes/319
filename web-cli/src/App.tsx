import {
  Box,
  SimpleGrid,
  Spacer,
  Text,
  Flex,
  Heading,
  VStack,
  useToast,
} from "@chakra-ui/react";
import Help from "./components/Help";
import { Constraint, IComponent, Method, ResponseJSON } from "./types";
import Component from "./components/Component";
import Terminal from "./components/Terminal";
import ScrollBox from "./components/ScrollBox";
import { useEffect, useState } from "react";
import { stringToColor } from "./util";
import { postDefault } from "./api";
import ConstraintDisplay from "./components/ConstraintDisplay";

const App = () => {
  const [components, setComponents] = useState<IComponent[]>([]);
  const [constraints, setConstraints] = useState<Constraint[]>([]);
  const [intercalatingConstraints, setIntercalatingConstraints] = useState<
    Constraint[]
  >([]);
  const [highlighted, setHighlighted] = useState<string[]>([]);
  const [enforceFromIndex, setEnforceFromIndex] = useState<number>(0);
  const toast = useToast();

  useEffect(() => {
    const enforceConstraints = async () => {
      try {
        const res = await postDefault(
          components,
          constraints,
          intercalatingConstraints,
          enforceFromIndex
        );

        if (res.status !== 200) {
          toast({
            title: "Error",
            description: "Something went wrong",
            status: "error",
            duration: 9000,
            isClosable: true,
          });
        }

        const json: ResponseJSON = await res.json();

        setComponents((prevComponents) => {
          const newComponents = prevComponents.map((comp) => {
            const newVariables = json.vars[comp.compID];
            return {
              ...comp,
              variables: newVariables.slice().sort((a, b) => {
                const aIndex = comp.variables.findIndex(
                  (variable) => variable.varID === a.varID
                );
                const bIndex = comp.variables.findIndex(
                  (variable) => variable.varID === b.varID
                );
                return aIndex - bIndex;
              }),
            };
          });
          if (
            JSON.stringify(newComponents) !== JSON.stringify(prevComponents)
          ) {
            return newComponents;
          } else {
            return prevComponents;
          }
        });
      } catch (e) {
        console.error(e);
        toast({
          title: "Error",
          description:
            "Could not reach the server, check the console for errors",
          status: "error",
          duration: 8000,
          isClosable: true,
        });
      }
    };
    if (
      components.length > 0 &&
      (constraints.length > 0 || intercalatingConstraints.length > 0)
    )
      enforceConstraints();
  }, [components, constraints, intercalatingConstraints, enforceFromIndex]);

  return (
    <Box bg="gray.800" h="100vh" p="0">
      <header className="App-header">
        <Box bg="gray.700" p="5" shadow="md">
          <Flex>
            <Heading size="lg">HotDrink / WarmDrink CLI</Heading>
            <Spacer />
            <Help />
          </Flex>
        </Box>
      </header>
      <Box as="main" h="calc(100vh - 170px)">
        <SimpleGrid columns={2} spacing={10} m="5" h="100%">
          <ScrollBox>
            <SimpleGrid columns={2}>
              <VStack align="left" mt="5" gap="5">
                <Heading size="md">Components</Heading>
                {components.length === 0 && (
                  <Text color="gray.500">No components</Text>
                )}
                {components.map((comp) => (
                  <Component
                    key={comp.compID}
                    compID={comp.compID}
                    variables={comp.variables}
                    strength={comp.strength}
                    highlighted={highlighted}
                  />
                ))}
              </VStack>
              <VStack align="left">
                <Heading size="md">Local Constraints</Heading>
                {constraints.length === 0 && (
                  <Text color="gray.500">No constraints</Text>
                )}
                {constraints.map((con: Constraint) => (
                  <ConstraintDisplay
                    key={con.constraintName}
                    constraint={con}
                    highlighted={highlighted}
                    isHighlighted={highlighted.includes(con.constraintName)}
                    highlightColor={stringToColor(con.constraintName)}
                  />
                ))}
                <Heading size="md">Intercalating Constraints</Heading>
                {intercalatingConstraints.length === 0 && (
                  <Text color="gray.500">No constraints</Text>
                )}
                {intercalatingConstraints.map((inter) => (
                  <ConstraintDisplay
                    key={inter.constraintName}
                    constraint={inter}
                    highlighted={highlighted}
                    isHighlighted={highlighted.includes(inter.constraintName)}
                    highlightColor={stringToColor(inter.constraintName)}
                  />
                ))}
              </VStack>
            </SimpleGrid>
          </ScrollBox>
          <Box>
            <Terminal
              components={components}
              setComponents={setComponents}
              setEnforceFromIndex={setEnforceFromIndex}
              setConstraints={setConstraints}
              setIntercalatingConstraints={setIntercalatingConstraints}
              setHighlighted={setHighlighted}
            />
          </Box>
        </SimpleGrid>
      </Box>
      <Flex as="footer" justifyContent="center" w="100%">
        <Text color="gray.500" p="5">
          Made with ❤️ by Bo Aanes
        </Text>
      </Flex>
    </Box>
  );
};

export default App;
