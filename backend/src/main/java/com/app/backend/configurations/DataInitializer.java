package com.app.backend.configurations;

import com.app.backend.services.VatPhamService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * Component khởi tạo dữ liệu mặc định khi ứng dụng start
 * - Khởi tạo vật phẩm (power-ups) mặc định
 */
@Component
@RequiredArgsConstructor
@Slf4j
@Order(100) // Chạy sau các config khác
public class DataInitializer implements ApplicationRunner {

    private final VatPhamService vatPhamService;

    @Override
    public void run(ApplicationArguments args) {
        log.info("🚀 Initializing default data...");

        try {
            // Khởi tạo vật phẩm mặc định
            vatPhamService.initDefaultItems();
            log.info("✅ Default items initialized successfully");
        } catch (Exception e) {
            log.error("❌ Error initializing default items: {}", e.getMessage());
        }
    }
}
