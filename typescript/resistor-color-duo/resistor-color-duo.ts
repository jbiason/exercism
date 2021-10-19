let bandColors: { [key: string]: number } = {
  'black': 0,
  'brown': 1,
  'red': 2,
  'orange': 3,
  'yellow': 4,
  'green': 5,
  'blue': 6,
  'violet': 7,
  'grey': 8,
  'white': 9,
};

export function decodedValue(colorArray: string[]): number {
  var firstBand:string = colorArray[0];
  var secondBand:string = colorArray[1];

  return bandColors[firstBand] * 10 + bandColors[secondBand];
}
