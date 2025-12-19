package com.app.backend.handlers;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.responses.ResponseObject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice // Dùng cái này thay cho @ControllerAdvice để chuyên cho REST API
public class GlobalExceptionHandler {

    // 1. Bắt lỗi DataNotFoundException (như bạn đã viết)
    @ExceptionHandler(DataNotFoundException.class)
    public ResponseEntity<ResponseObject> handleDataNotFoundException(DataNotFoundException ex) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND) // Thường DataNotFound trả về 404 Not Found hợp lý hơn 400
                .body(ResponseObject.builder()
                        .status(HttpStatus.NOT_FOUND)
                        .data(null)
                        .message(ex.getMessage())
                        .build());
    }

    // 2. BẮT BUỘC PHẢI CÓ CÁI NÀY ĐỂ BẮT LỖI LOGIC BỘ CÂU HỎI
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ResponseObject> handleIllegalArgumentException(IllegalArgumentException ex) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST) // Trả về 400 Bad Request
                .body(ResponseObject.builder()
                        .status(HttpStatus.BAD_REQUEST)
                        .data(null)
                        .message(ex.getMessage()) // Lấy message: "Bộ câu hỏi phải có ít nhất 5..."
                        .build());
    }

    // ✅ 3. Xử lý PermissionDenyException (403 Forbidden)
    @ExceptionHandler(PermissionDenyException.class)
    public ResponseEntity<ResponseObject> handlePermissionDenyException(PermissionDenyException ex) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN) // Trả về 403 Forbidden
                .body(ResponseObject.builder()
                        .status(HttpStatus.FORBIDDEN)
                        .data(null)
                        .message(ex.getMessage())
                        .build());
    }

    // 4. Xử lý lỗi chung chung (Exception.class) để tránh lỗi 500 xấu xí không rõ nguyên nhân
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ResponseObject> handleGeneralException(Exception ex) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body(ResponseObject.builder()
                        .status(HttpStatus.INTERNAL_SERVER_ERROR)
                        .data(null)
                        .message("Lỗi hệ thống: " + ex.getMessage()) // Hoặc message chung chung
                        .build());
    }
}