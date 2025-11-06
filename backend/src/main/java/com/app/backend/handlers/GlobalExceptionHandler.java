package com.app.backend.handlers;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.responses.ResponseObject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

// Đặt trong một gói riêng (ví dụ: com.app.backend.handlers)
@ControllerAdvice
public class GlobalExceptionHandler {

    // Bắt DataNotFoundException (hoặc Exception tùy theo tên bạn đặt)
    @ExceptionHandler(DataNotFoundException.class)
    public ResponseEntity<ResponseObject> handleDataNotFoundException(DataNotFoundException ex) {
        // Lấy thông báo lỗi chính xác từ Exception
        String errorMessage = ex.getMessage();

        // Tùy chỉnh HttpStatus và ResponseObject
        // BAD_REQUEST (400) hoặc CONFLICT (409) có thể phù hợp hơn cho lỗi "already exists"
        return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(ResponseObject.builder()
                        .status(HttpStatus.BAD_REQUEST)
                        .data(null)
                        .message(errorMessage) // Chỉ lấy "Account already exists"
                        .build());
    }

    // Bạn vẫn giữ phần xử lý validation lỗi (BindingResult) trong Controller,
    // hoặc chuyển sang bắt MethodArgumentNotValidException ở đây để gộp lỗi.

    // ... các Exception Handler khác
}
