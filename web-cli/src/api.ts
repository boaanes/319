import { IComponent, Constraint, ResponseJSON } from "./types";

export const postDefault = async (
  components: IComponent[],
  constraints: Constraint[],
  intercalatingConstraints: Constraint[],
  enforceFromIndex: number
): Promise<Response> => {
  const formattedComps = components.map((component) => {
    return {
      ...component,
      strength: component.variables
        .map((variable) => variable.varID)
        .slice()
        .reverse(),
    };
  });

  const formattedConstraints = constraints.map((constraint) => {
    return {
      ...constraint,
      methods: constraint.methods.map((method) => {
        return {
          ...method,
          expressions: method.expressions.filter(
            (expression) => !!expression[1]
          ),
        };
      }),
    };
  });

  const formattedIntercalatingConstraints = intercalatingConstraints.map(
    (constraint) => {
      return {
        ...constraint,
        methods: constraint.methods.map((method) => {
          return {
            ...method,
            expressions: method.expressions.filter(
              (expression) => !!expression[1]
            ),
          };
        }),
      };
    }
  );

  return fetch("http://localhost:8000/", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      components: formattedComps,
      constraints: formattedConstraints,
      intercalatingConstraints: formattedIntercalatingConstraints,
      enforceFromIndex,
    }),
  });
};
