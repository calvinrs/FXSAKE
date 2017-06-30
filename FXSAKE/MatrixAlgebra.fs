namespace FxSake

open MathNet.Numerics.LinearAlgebra

module FXSEMatrixAlgebra = 
    
    let isPSD (matrix: Matrix<float>) =
        if matrix.RowCount = matrix.ColumnCount then         
            matrix.Evd().EigenValues.Real() |> Seq.min >= 0.000001
        else 
            false
    
    let eigenValues (matrix: Matrix<float>) =
        if matrix.RowCount = matrix.ColumnCount then         
            matrix.Evd().EigenValues.Real().ToArray()
        else 
            failwith "Matrix is not square."
    
    // Wrappers using the array representation of a matrix
    let xlsEigenValues (myMatrix: float[,]) =
        let thisMatrix = DenseMatrix.ofArray2 myMatrix
        eigenValues thisMatrix

    let xlsMatrixIsPSD (myMatrix: float[,]) =
        let thisMatrix = DenseMatrix.ofArray2 myMatrix
        isPSD thisMatrix