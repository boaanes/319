import { Box, BoxProps } from "@chakra-ui/react";
import styled from "@emotion/styled";
import { useEffect, useRef } from "react";

const StyledBox = styled(Box)`
  ::-webkit-scrollbar {
    width: 8px;
  }

  ::-webkit-scrollbar-track {
    background: transparent;
  }

  ::-webkit-scrollbar-thumb {
    background: #4a5568;
    border-radius: 4px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #4a5568;
  }
`;

interface Props extends BoxProps {
  children: React.ReactNode;
  keepAtBottom?: boolean;
}

const ScrollBox = ({ children, keepAtBottom, ...props }: Props) => {
  const divRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    if (divRef.current !== null) {
      divRef.current.scrollTop = divRef.current.scrollHeight;
    }
  });

  return (
    <StyledBox
      bg="gray.700"
      p="5"
      shadow="md"
      borderRadius="10"
      mb="5"
      maxH="100%"
      overflowY="auto"
      ref={keepAtBottom ? divRef : undefined}
      {...props}
    >
      {children}
    </StyledBox>
  );
};

export default ScrollBox;
