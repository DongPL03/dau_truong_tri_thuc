package com.app.backend.controllers;

import com.app.backend.dtos.SuDungVatPhamDTO;
import com.app.backend.models.BattleState;
import com.app.backend.models.VatPham;
import com.app.backend.responses.SuDungVatPhamResponse;
import com.app.backend.responses.VatPhamInventoryResponse;
import com.app.backend.services.TranDauService;
import com.app.backend.services.VatPhamService;
import com.app.backend.components.JwtTokenUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("${api.prefix}/vat-pham")
@RequiredArgsConstructor
@Slf4j
public class VatPhamController {

    private final VatPhamService vatPhamService;
    private final TranDauService tranDauService;
    private final JwtTokenUtils jwtTokenUtils;

    /**
     * Lấy inventory của user hiện tại
     */
    @GetMapping("/inventory")
    public ResponseEntity<List<VatPhamInventoryResponse>> getInventory(
            @RequestHeader("Authorization") String authHeader) {
        try {
            Long userId = jwtTokenUtils.extractUserId(authHeader);
            List<VatPhamInventoryResponse> inventory = vatPhamService.getInventory(userId);
            return ResponseEntity.ok(inventory);
        } catch (Exception e) {
            log.error("Error getting inventory", e);
            return ResponseEntity.badRequest().build();
        }
    }

    /**
     * Lấy danh sách tất cả vật phẩm (cho shop/display)
     */
    @GetMapping("/all")
    public ResponseEntity<List<VatPham>> getAllItems() {
        return ResponseEntity.ok(vatPhamService.getAllActiveItems());
    }

    /**
     * Sử dụng vật phẩm trong trận đấu
     */
    @PostMapping("/use")
    public ResponseEntity<?> useItem(
            @RequestHeader("Authorization") String authHeader,
            @RequestBody SuDungVatPhamDTO dto) {
        try {
            Long userId = jwtTokenUtils.extractUserId(authHeader);

            // Lấy BattleState hiện tại
            BattleState battleState = tranDauService.getState(dto.getTranDauId());
            if (battleState == null) {
                return ResponseEntity.badRequest()
                        .body(Map.of("message", "Trận đấu không tồn tại hoặc chưa bắt đầu"));
            }

            // Kiểm tra user có trong trận không
            if (!battleState.getDiemNguoiChoi().containsKey(userId)) {
                return ResponseEntity.badRequest()
                        .body(Map.of("message", "Bạn không tham gia trận đấu này"));
            }

            // Kiểm tra trận đã kết thúc chưa
            if (battleState.isFinished()) {
                return ResponseEntity.badRequest()
                        .body(Map.of("message", "Trận đấu đã kết thúc"));
            }

            SuDungVatPhamResponse response = vatPhamService.useItem(userId, dto, battleState);

            // Nếu sử dụng thành công, broadcast event cho các player khác
            if (response.isThanhCong()) {
                tranDauService.broadcastItemUsed(dto.getTranDauId(), userId, response);
            }

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("Error using item", e);
            return ResponseEntity.badRequest()
                    .body(Map.of("message", e.getMessage()));
        }
    }

    /**
     * Admin: Tặng vật phẩm cho user
     */
    @PostMapping("/grant")
    public ResponseEntity<?> grantItem(
            @RequestParam Long userId,
            @RequestParam Long vatPhamId,
            @RequestParam(defaultValue = "1") int quantity) {
        try {
            vatPhamService.grantItemToUser(userId, vatPhamId, quantity);
            return ResponseEntity.ok(Map.of(
                    "message", "Đã tặng vật phẩm thành công",
                    "user_id", userId,
                    "vat_pham_id", vatPhamId,
                    "quantity", quantity
            ));
        } catch (Exception e) {
            log.error("Error granting item", e);
            return ResponseEntity.badRequest()
                    .body(Map.of("message", e.getMessage()));
        }
    }

    /**
     * Admin: Khởi tạo vật phẩm mặc định
     */
    @PostMapping("/init-defaults")
    public ResponseEntity<?> initDefaults() {
        try {
            vatPhamService.initDefaultItems();
            return ResponseEntity.ok(Map.of("message", "Đã khởi tạo vật phẩm mặc định"));
        } catch (Exception e) {
            log.error("Error initializing default items", e);
            return ResponseEntity.badRequest()
                    .body(Map.of("message", e.getMessage()));
        }
    }
}
