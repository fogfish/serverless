//
// Copyright (C) 2019 Dmitry Kolesnikov
//
// This pure.ts file may be modified and distributed under the terms
// of the MIT license.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//   
import * as cdk from '@aws-cdk/core'

export type IaaC<T> = (parent: cdk.Construct) => T

export function _<T>(parent: cdk.Construct, ...fns: Array<IaaC<T>>): cdk.Construct {
  fns.forEach(
    fn => {
    parent instanceof cdk.App 
      ? fn(new cdk.Stack(parent, fn.name))
      : fn(new cdk.Construct(parent, fn.name))
    }
  )
  return parent
}

export function $<T>(parent: cdk.App, fn: IaaC<T>, name?: string): cdk.Construct {
  fn(new cdk.Stack(parent, name || fn.name))
  return parent
}
