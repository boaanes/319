export interface Method {
  methodName: string;
  inputs: string[];
  expressions: Array<[string, string | undefined]>;
}

export interface Constraint {
  constraintName: string;
  methods: Method[];
}

export interface Variable {
  varID: string;
  varValue: number | boolean;
}

export interface IComponent {
  compID: number;
  variables: Variable[];
  strength: string[];
}

export interface ComponentList {
  components: IComponent[];
}

export interface ResponseJSON {
  vars: {
    [key: string]: [Variable];
  };
}
