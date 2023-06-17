import {
  Text,
  Flex,
  Input,
  InputGroup,
  InputLeftElement,
  useDisclosure,
} from "@chakra-ui/react";
import { useEffect, useRef, useState } from "react";
import ScrollBox from "./ScrollBox";
import { Constraint, IComponent } from "../types";
import {
  getComponentWithHighestID,
  getFirst,
  insertAfter,
  swapComponents,
} from "../util";
import ConstraintDialog from "./ConstraintModal";

interface Props {
  components: IComponent[];
  setComponents: React.Dispatch<React.SetStateAction<IComponent[]>>;
  setEnforceFromIndex: React.Dispatch<React.SetStateAction<number>>;
  setConstraints: React.Dispatch<React.SetStateAction<Constraint[]>>;
  setIntercalatingConstraints: React.Dispatch<
    React.SetStateAction<Constraint[]>
  >;
  setHighlighted: React.Dispatch<React.SetStateAction<string[]>>;
}

const commands = [
  "newComp",
  "newList",
  "newVar",
  "newCtrn",
  "newIctrn",
  "updVar",
  "delVar",
  "insAfter",
  "swap",
  "rmv",
  "highlight",
  "reset",
  "help",
];

const formatResponse = (command: string) => `> ${command}`;
const formatInput = (command: string) => `$ ${command}`;

const Terminal = ({
  components,
  setComponents,
  setEnforceFromIndex,
  setConstraints,
  setIntercalatingConstraints,
  setHighlighted,
}: Props) => {
  const [input, setInput] = useState("");
  const [history, setHistory] = useState<string[]>([]);

  const { isOpen, onOpen, onClose } = useDisclosure();
  const [mode, setMode] = useState<"local" | "intercalating">("local");
  const initialRef = useRef<HTMLInputElement>(null);
  const finalRef = useRef<HTMLInputElement>(null);
  const handleOpenConstraintModal = (mode: "local" | "intercalating") => {
    setMode(mode);
    onOpen();
  };

  useEffect(() => {
    if (isOpen && initialRef.current) {
      initialRef.current.focus();
    } else if (!isOpen && finalRef.current) {
      finalRef.current.focus();
    }
  }, [initialRef, isOpen]);

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (input === "") return;
    const [command, ...args] = input.split(" ").filter((word) => word !== "");

    if (!commands.includes(command)) {
      setHistory((prevHistory) => [
        ...prevHistory,
        formatInput(input),
        formatResponse("Invalid command"),
      ]);
      setInput("");
      return;
    }

    let response = "";

    switch (command) {
      case "newComp": {
        if (components.length === 0) {
          const newComp: IComponent = {
            compID: 0,
            variables: [],
            strength: [],
          };
          setComponents((prevComponents) => [...prevComponents, newComp]);
          response = `Created component ${newComp.compID}`;
          break;
        }
        const newComp: IComponent = {
          compID:
            components.reduce((acc, curr) => Math.max(acc, curr.compID), 0) + 1,
          variables: components[0].variables.map((variable) => ({
            ...variable,
          })),
          strength: components[0].strength,
        };
        setEnforceFromIndex(Math.max(components.length - 1, 0));
        setComponents((prevComponents) => [...prevComponents, newComp]);
        response = `Created component ${newComp.compID}`;
        break;
      }
      case "newList": {
        const [n] = args;
        if (n === undefined) {
          response = "Invalid number of components";
          break;
        }
        const num = parseInt(n);
        if (isNaN(num)) {
          response = "Invalid number of components";
          break;
        }
        if (num < 1) {
          response = "Invalid number of components";
          break;
        }
        const newComps: IComponent[] = [];
        const startID =
          components.reduce((acc, curr) => Math.max(acc, curr.compID), 0) + 1 ||
          0;
        for (let i = startID; i < startID + num; i++) {
          const newComp: IComponent = {
            compID: i,
            variables:
              components.length > 0
                ? components[0].variables.map((variable) => ({
                    ...variable,
                  }))
                : [],
            strength: components.length > 0 ? components[0].strength : [],
          };
          newComps.push(newComp);
        }
        setEnforceFromIndex(Math.max(components.length - 1, 0));
        setComponents((prevComponents) => [...prevComponents, ...newComps]);
        response = `Created ${num} components`;
        break;
      }
      case "newVar": {
        const [identifier, value] = args;
        if (identifier === undefined || value === undefined) {
          response = "Invalid variable";
          break;
        }
        const num = parseInt(value);
        if (isNaN(num)) {
          const bool =
            value === "true" ? true : value === "false" ? false : undefined;
          if (bool === undefined) {
            response = "Invalid variable";
            break;
          }
          setEnforceFromIndex(0);
          setComponents((prevComponents) =>
            prevComponents.map((component) => {
              return {
                ...component,
                variables: [
                  ...component.variables.filter(
                    (variable) => variable.varID !== identifier
                  ),
                  {
                    varID: identifier,
                    varValue: bool,
                  },
                ],
              };
            })
          );
          response = `Created variable ${identifier} with value ${bool}`;
          break;
        }
        setEnforceFromIndex(0);
        setComponents((prevComponents) =>
          prevComponents.map((component) => {
            return {
              ...component,
              variables: [
                ...component.variables.filter(
                  (variable) => variable.varID !== identifier
                ),
                {
                  varID: identifier,
                  varValue: num,
                },
              ],
            };
          })
        );
        response = `Created variable ${identifier} with value ${num}`;
        break;
      }
      case "newCtrn": {
        if (components.length === 0) {
          response = "no components to constrain";
          break;
        }
        handleOpenConstraintModal("local");
        break;
      }
      case "newIctrn": {
        if (components.length === 0) {
          response = "no components to constrain";
          break;
        }
        handleOpenConstraintModal("intercalating");
        break;
      }
      case "updVar": {
        const [compID, varID, value] = args;
        const comp = components.find(
          (component) => component.compID === parseInt(compID)
        );
        if (
          compID === undefined ||
          varID === undefined ||
          value === undefined
        ) {
          response = "Invalid variable";
          break;
        }
        if (comp === undefined) {
          response = "Invalid component ID";
          break;
        }
        const num = parseInt(value);
        if (isNaN(num)) {
          const bool =
            value === "true" ? true : value === "false" ? false : undefined;
          if (bool === undefined) {
            response = "Invalid variable";
            break;
          }
          setEnforceFromIndex(Math.max(components.indexOf(comp) - 1, 0));
          setComponents((prevComponents) =>
            prevComponents.map((component) => {
              if (component.compID !== parseInt(compID)) return component;
              return {
                ...component,
                variables: [
                  ...component.variables.filter(
                    (variable) => variable.varID !== varID
                  ),
                  {
                    varID: varID,
                    varValue: num,
                  },
                ],
              };
            })
          );
          response = `Updated variable ${varID} with value ${bool}`;
          break;
        }
        setEnforceFromIndex(Math.max(components.indexOf(comp) - 1, 0));
        setComponents((prevComponents) =>
          prevComponents.map((component) => {
            if (component.compID !== parseInt(compID)) return component;
            return {
              ...component,
              variables: [
                ...component.variables.filter(
                  (variable) => variable.varID !== varID
                ),
                {
                  varID: varID,
                  varValue: num,
                },
              ],
            };
          })
        );
        response = `Updated variable ${varID} with value ${num}`;
        break;
      }
      case "delVar": {
        const [varID] = args;
        if (varID === undefined) {
          response = "Invalid variable";
          break;
        }
        setEnforceFromIndex(0);
        setComponents((prevComponents) =>
          prevComponents.map((component) => {
            return {
              ...component,
              variables: component.variables.filter(
                (variable) => variable.varID !== varID
              ),
            };
          })
        );
        response = `Deleted variable ${varID}`;
        break;
      }
      case "insAfter": {
        const [compID] = args;
        if (compID === undefined) {
          response = "Invalid component ID";
          break;
        }
        const preceedingComp = components.find(
          (component) => component.compID === parseInt(compID)
        );
        if (preceedingComp === undefined) {
          response = "Invalid component ID";
          break;
        }
        const newComp = {
          compID: getComponentWithHighestID(components).compID + 1,
          variables: preceedingComp.variables.map((variable) => {
            return {
              varID: variable.varID,
              varValue: variable.varValue,
            };
          }),
          strength: preceedingComp.strength.slice(),
        };
        setEnforceFromIndex(components.indexOf(preceedingComp));
        setComponents((prevComponents) =>
          insertAfter(
            prevComponents,
            prevComponents.indexOf(preceedingComp),
            newComp
          )
        );
        response = `Inserted new component after ${compID}`;
        break;
      }
      case "swap": {
        const [compID1, compID2] = args;
        if (compID1 === undefined || compID2 === undefined) {
          response = "Invalid component ID";
          break;
        }
        const comp1 = components.find(
          (component) => component.compID === parseInt(compID1)
        );
        const comp2 = components.find(
          (component) => component.compID === parseInt(compID2)
        );
        if (comp1 === undefined || comp2 === undefined) {
          response = "Invalid component ID";
          break;
        }
        setEnforceFromIndex(
          Math.max(
            components.indexOf(getFirst(components, comp1, comp2)) - 1,
            0
          )
        );
        setComponents((prevComponents) =>
          swapComponents(prevComponents, comp1, comp2)
        );
        response = `Swapped ${compID1} and ${compID2}`;
        break;
      }
      case "rmv": {
        const [compID] = args;
        if (compID === undefined) {
          response = "Invalid component ID";
          break;
        }
        const comp = components.find(
          (component) => component.compID === parseInt(compID)
        );
        if (comp === undefined) {
          response = "Invalid component ID";
          break;
        }
        setEnforceFromIndex(Math.max(components.indexOf(comp) - 1, 0));
        setComponents((prevComponents) =>
          prevComponents.filter(
            (component) => component.compID !== parseInt(compID)
          )
        );
        response = `Removed component ${compID}`;
        break;
      }
      case "highlight": {
        const [id] = args;
        if (id === undefined) {
          response = "Invalid identifier";
          break;
        }
        setHighlighted((prevHighlighted) => [...prevHighlighted, id]);
        response = `Highlighted ${id}`;
        break;
      }
      case "reset": {
        setHighlighted([]);
        response = "Reset highlights";
        break;
      }
      case "help": {
        response = `Commands:
            addComp - Add a new component
            addVar <varID> <value> - Add a new variable to all components
            updVar <compID> <varID> <value> - Update the given variable
            delVar <varID> - Delete the given variable
            insAfter <compID> - Insert a new component after the given component
            swap <compID> <compID> - Swap the given components
            rmv <compID> - Remove the given component
            highlight <identifier> - Highlight the element for the given identifier
            help - Display this help message`;
        break;
      }
      default: {
        response = "Invalid command";
        return;
      }
    }

    setHistory((prevHistory) => [
      ...prevHistory,
      formatInput(input),
      formatResponse(response),
    ]);
    setInput("");
  };

  return (
    <Flex direction="column" h="calc(100vh - 170px)" fontFamily="monospace">
      <ScrollBox keepAtBottom flex="1" w="100%">
        {isOpen ? (
          <ConstraintDialog
            onClose={onClose}
            components={components}
            setConstraints={setConstraints}
            setIntercalatingConstraints={setIntercalatingConstraints}
            setEnforceFromIndex={setEnforceFromIndex}
            mode={mode}
            initialRef={initialRef}
          />
        ) : (
          <>
            {history.map((command, index) => (
              <Text key={index}>{command}</Text>
            ))}
          </>
        )}
      </ScrollBox>
      <form onSubmit={submit}>
        <InputGroup w="100%" bg="gray.700" shadow="md" borderRadius="10">
          <InputLeftElement children="$" />
          <Input
            value={input}
            onChange={(e) => setInput(e.target.value)}
            ref={finalRef}
          />
        </InputGroup>
      </form>
    </Flex>
  );
};

export default Terminal;
