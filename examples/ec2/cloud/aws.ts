import * as cdk from '@aws-cdk/core'
import * as iam from '@aws-cdk/aws-iam'
import * as lambda from '@aws-cdk/aws-lambda'
import * as logs from '@aws-cdk/aws-logs'
import { _, $ } from './pure'

//
//
function Lambda(parent: cdk.Construct): lambda.Function {
  return new lambda.Function(parent, 'EC2',
    {
      description: 'Erlang AWS Lambda - EC2 API Example',
      runtime: lambda.Runtime.PROVIDED,
      code: new lambda.AssetCode('../_build/default/bin'),
      handler: 'index.handler',
      role: Role(parent),
      timeout: cdk.Duration.seconds(10),
      memorySize: 256,
      logRetention: logs.RetentionDays.FIVE_DAYS,
      layers: [
        lambda.LayerVersion.fromLayerVersionArn(parent, 'Layer', 
          `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:erlang-serverless:3`)
      ]
    }
  )
}

//
//
function Role(parent: cdk.Construct): iam.Role {
  const role = new iam.Role(parent, 'Role',
    {
      assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
    }
  )
  role.addManagedPolicy(iam.ManagedPolicy.fromAwsManagedPolicyName("AmazonEC2ReadOnlyAccess"))
  return role
}

function ExampleErlEC2(stack: cdk.Construct): cdk.Construct {
  _(stack, Lambda)
  return stack
}

const app = new cdk.App()
_(app, ExampleErlEC2)
app.synth()