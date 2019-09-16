import * as cdk from '@aws-cdk/core'
import * as iam from '@aws-cdk/aws-iam'
import * as lambda from '@aws-cdk/aws-lambda'
import * as logs from '@aws-cdk/aws-logs'
import { IaaC, iaac, root, join, use } from 'aws-cdk-pure'

const f = iaac(lambda.Function)
const role = iaac(iam.Role)

//
//
function Lambda(parent: cdk.Construct): lambda.FunctionProps {
  return {
    description: 'Erlang AWS Lambda - EC2 API Example',
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../_build/default/bin'),
    handler: 'index.handler',
    role: Role()(parent),
    timeout: cdk.Duration.seconds(10),
    memorySize: 256,
    logRetention: logs.RetentionDays.FIVE_DAYS,
    layers: [
      lambda.LayerVersion.fromLayerVersionArn(parent, 'Layer', 
        `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:erlang-serverless:3`)
    ]
  }
}

//
//
function Role(): IaaC<iam.Role> {
  return use({role: role(Principal)})
    .effect(x => x.role.addManagedPolicy(Allow()))
    .yield('role')
}

function Principal(): iam.RoleProps {
  return {
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  }
}

function Allow(): iam.IManagedPolicy {
  return iam.ManagedPolicy.fromAwsManagedPolicyName("AmazonEC2ReadOnlyAccess")
}

//
//
function ExampleErlEC2(stack: cdk.Construct): cdk.Construct {
  join(stack, f(Lambda))
  return stack
}

const app = new cdk.App()
root(app, ExampleErlEC2)
app.synth()