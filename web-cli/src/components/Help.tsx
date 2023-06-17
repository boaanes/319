import * as React from "react";
import {
  Button,
  Icon,
  Modal,
  ModalOverlay,
  ModalContent,
  ModalHeader,
  ModalCloseButton,
  ModalBody,
  ModalFooter,
  useDisclosure,
  UnorderedList,
  ListItem,
} from "@chakra-ui/react";
import { HelpCircle } from "react-feather";

const HelpModal: React.FC = () => {
  const { isOpen, onOpen, onClose } = useDisclosure();

  return (
    <>
      <Button leftIcon={<Icon as={HelpCircle} />} onClick={onOpen}>
        Help
      </Button>

      <Modal isOpen={isOpen} onClose={onClose}>
        <ModalOverlay />
        <ModalContent>
          <ModalHeader>Help</ModalHeader>
          <ModalCloseButton />
          <ModalBody>
            <UnorderedList>
              <ListItem>
                Lorem ipsum dolor sit amet, consectetur adipiscing elit
              </ListItem>
              <ListItem>
                Ut enim ad minim veniam, quis nostrud exercitation ullamco
                laboris
              </ListItem>
              <ListItem>
                Duis aute irure dolor in reprehenderit in voluptate velit esse
                cillum dolore
              </ListItem>
              <ListItem>
                Excepteur sint occaecat cupidatat non proident, sunt in culpa
                qui officia deserunt mollit anim id est laborum
              </ListItem>
            </UnorderedList>
          </ModalBody>

          <ModalFooter>
            <Button colorScheme="blue" mr={3} onClick={onClose}>
              Close
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  );
};

export default HelpModal;
