import { IComponent, Method } from "./types";
import * as convert from "color-convert";

export function prettyPrintMethod(method: Method): string {
  let result = "";
  result +=
    "[" +
    method.inputs.join(", ") +
    "] -> " +
    method.methodName +
    " -> [" +
    method.expressions
      .filter((e) => e[1] !== "")
      .map((e) => e[0])
      .join(", ") +
    "]";
  return result;
}

export function getComponentWithHighestID(
  components: IComponent[]
): IComponent {
  let maxID = 0;
  let maxComponent = components[0];
  for (let component of components) {
    if (component.compID > maxID) {
      maxID = component.compID;
      maxComponent = component;
    }
  }
  return maxComponent;
}

export function insertAfter(array: any[], index: number, newItem: any): any[] {
  return [...array.slice(0, index + 1), newItem, ...array.slice(index + 1)];
}

export function getFirst(
  components: IComponent[],
  compA: IComponent,
  compB: IComponent
): IComponent {
  let compAIndex = components.indexOf(compA);
  let compBIndex = components.indexOf(compB);
  if (compAIndex < compBIndex) {
    return compA;
  } else {
    return compB;
  }
}

export function swapComponents(
  components: IComponent[],
  compA: IComponent,
  compB: IComponent
): IComponent[] {
  let compAIndex = components.indexOf(compA);
  let compBIndex = components.indexOf(compB);
  let newComponents = components.slice();
  newComponents[compAIndex] = compB;
  newComponents[compBIndex] = compA;
  return newComponents;
}

// convert a string to a color
// https://stackoverflow.com/questions/3426404/create-a-hexadecimal-colour-based-on-a-string-with-javascript
export function stringToColor(input: string): string {
  let hash = 7; // a prime number
  for (let i = 0; i < input.length; i++) {
    hash = hash * 31 + input.charCodeAt(i); // use 31, another prime number
    hash = hash & hash; // Convert to 32bit integer
  }

  // Calculate hue based on the hash. Since hue can be between 0 and 360, we mod by 360
  const hue = Math.abs(hash) % 360;

  // We keep saturation and lightness high to ensure the color is bright.
  const saturation = 90; // Percentage
  const lightness = 70; // Percentage

  return `hsl(${hue},${saturation}%,${lightness}%)`;
}
