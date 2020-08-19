// https://adventofcode.com/2019/day/4 in Dart.

import 'dart:math';

List<int> passwordDigits(int n) {
  return [for (var k = 5; k >= 0; k--) n ~/ pow(10, k) % 10];
}

enum Validity { Invalid, Valid, StrictlyValid }

Validity checkPassword(int n) {
  final d = passwordDigits(n);
  var foundPair = false;
  var foundStrictPair = false;

  for (var i = 0; i < d.length - 1; i++) {
    // Disallow descending pairs.
    if (d[i] > d[i + 1]) return Validity.Invalid;

    // Require at least one pair.
    if (d[i] == d[i + 1]) {
      foundPair = true;
      final extendsLeft = (i >= 1 && d[i] == d[i - 1]);
      final extendsRight = (i + 2 < d.length && d[i + 1] == d[i + 2]);
      if (!extendsLeft && !extendsRight) foundStrictPair = true;
    }
  }

  if (foundStrictPair) {
    return Validity.StrictlyValid;
  } else if (foundPair) {
    return Validity.Valid;
  } else {
    return Validity.Invalid;
  }
}

void main(List<String> arguments) {
  final low = int.parse(arguments[0]);
  final high = int.parse(arguments[1]);
  var passwords = 0;
  var strictPasswords = 0;
  for (var i = low; i <= high; i++) {
    switch (checkPassword(i)) {
      case Validity.StrictlyValid:
        strictPasswords += 1;
        passwords += 1;
        break;
      case Validity.Valid:
        passwords += 1;
        break;
      default:
        break;
    }
  }
  print('*  $passwords passwords.');
  print('** $strictPasswords strict pair-only passwords.');
}
