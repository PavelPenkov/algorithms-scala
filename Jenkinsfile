pipeline {
  agent any
  stages {
    stage('do shit') {
      steps {
        sh 'echo "Hello"'
        input(message: 'Name?', id: 'name', ok: 'string')
      }
    }
  }
}