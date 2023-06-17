import {
  Text,
  InputGroup,
  Input,
  Button,
  VStack,
  Center,
  Flex,
  Heading,
  Divider,
  StackDivider,
} from "@chakra-ui/react";
import { useState } from "react";
import { IComponent, Constraint, Method } from "../types";
import MethodForm from "./MethodForm";

interface Props {
  onClose: () => void;
  components: IComponent[];
  setConstraints: React.Dispatch<React.SetStateAction<Constraint[]>>;
  setIntercalatingConstraints: React.Dispatch<
    React.SetStateAction<Constraint[]>
  >;
  setEnforceFromIndex: React.Dispatch<React.SetStateAction<number>>;
  mode: "local" | "intercalating";
  initialRef: React.MutableRefObject<HTMLInputElement | null>;
}

const ConstraintDialog = ({
  onClose,
  components,
  setConstraints,
  setIntercalatingConstraints,
  setEnforceFromIndex,
  mode,
  initialRef,
}: Props) => {
  const [name, setName] = useState("");
  const refComponent = components[0];
  const [methods, setMethods] = useState<Method[]>([]);

  const handleClose = () => {
    setName("");
    setMethods([]);
    onClose();
  };

  return (
    <>
      <Heading size="md" mb={5}>{`Create a new ${mode} constraint`}</Heading>
      <Text>Name</Text>
      <InputGroup mb={5}>
        <Input
          type="text"
          placeholder="Constraint name"
          value={name}
          ref={initialRef}
          onChange={(e) => setName(e.target.value)}
        />
      </InputGroup>
      <Text>Methods</Text>
      <VStack mb="5" align="left" divider={<StackDivider />}>
        {methods.map((method, index) => (
          <MethodForm
            key={index}
            variables={refComponent.variables}
            method={method}
            setMethods={setMethods}
          />
        ))}
      </VStack>
      <Button
        colorScheme="blue"
        onClick={() => {
          setMethods([
            ...methods,
            {
              methodName: "",
              inputs: [],
              expressions: refComponent.variables.map((variable) => [
                variable.varID,
                "",
              ]),
            },
          ]);
        }}
      >
        Add method
      </Button>
      <Divider my="3" />
      <Flex justifyContent="flex-end" gap="2" mt={5}>
        <Button colorScheme="blue" mr={3} onClick={handleClose}>
          Cancel
        </Button>
        <Button
          colorScheme="green"
          onClick={() => {
            if (name === "") return;
            if (mode === "local") {
              setEnforceFromIndex(0);
              setConstraints((prevConstraints) => [
                ...prevConstraints.filter(
                  (constraint) => constraint.constraintName !== name
                ),
                {
                  constraintName: name,
                  methods: methods,
                },
              ]);
            } else {
              setEnforceFromIndex(0);
              setIntercalatingConstraints((prevInter) => [
                ...prevInter.filter(
                  (constraint) => constraint.constraintName !== name
                ),
                {
                  constraintName: name,
                  methods: methods,
                },
              ]);
            }
            setName("");
            setMethods([]);
            onClose();
          }}
        >
          Create
        </Button>
      </Flex>
    </>
  );
};

export default ConstraintDialog;
