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
            matrix.Evd().EigenValues.Real()
        else 
            failwith "Matrix is not square."
    
    let xlsMatrixIsPSD (myMatrix: float[,]) =
        let thisMatrix = DenseMatrix.ofArray2 myMatrix
        isPSD thisMatrix