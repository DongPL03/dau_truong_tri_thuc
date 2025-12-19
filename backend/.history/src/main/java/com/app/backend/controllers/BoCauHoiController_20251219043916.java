package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.bocauhoi.BoCauHoiResponse;
import com.app.backend.responses.bocauhoi.UnlockBoCauHoiResponse;
import com.app.backend.services.bocauhoi.IBoCauHoiService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("${api.prefix}/boCauHoi")
@RequiredArgsConstructor
public class BoCauHoiController {
    private final IBoCauHoiService boCauHoiService;
    private final SecurityUtils securityUtils;

    @GetMapping("")
    public ResponseEntity<ResponseObject> getAll(
            @RequestParam(defaultValue = "") String keyword,
            @RequestParam(defaultValue = "0", name = "chu_de_id") Long chuDeId,
            @RequestParam(defaultValue = "", name = "che_do_hien_thi") String cheDoHienThi,
            @RequestParam(defaultValue = "", name = "trang_thai") String trangThai,
            @RequestParam(defaultValue = "NEWEST") String sort_order, // 🔹 thêm param mới
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        Sort sort = sort_order.equalsIgnoreCase("OLDEST")
                ? Sort.by(Sort.Direction.ASC, "taoLuc")
                : Sort.by(Sort.Direction.DESC, "taoLuc");
        PageRequest pageRequest = PageRequest.of(page, limit, sort);
        Page<BoCauHoi> result = boCauHoiService.findAllBoCauHoi(
                pageRequest,
                keyword,
                chuDeId,
                cheDoHienThi,
                trangThai,
                currentUserId,
                isAdmin
        );
        Page<BoCauHoiResponse> dtoPage = result.map(bo ->
                BoCauHoiResponse.from(
                        bo,
                        boCauHoiService.hasUserUnlockedBo(bo.getId(), currentUserId)
                )
        );
        // Đưa vào wrapper
        PageResponse<BoCauHoiResponse> data = PageResponse.fromPage(dtoPage);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/practice-sets")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getPracticeSets(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "100") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();

        // sort mới nhất trước
        PageRequest pageRequest = PageRequest.of(page, limit, Sort.by(Sort.Direction.DESC, "taoLuc"));

        Page<BoCauHoi> result = boCauHoiService.findPracticeSets(
                pageRequest,
                currentUserId,
                isAdmin
        );

        Page<BoCauHoiResponse> dtoPage = result.map(bo ->
                BoCauHoiResponse.from(
                        bo,
                        boCauHoiService.hasUserUnlockedBo(bo.getId(), currentUserId)
                )
        );
        PageResponse<BoCauHoiResponse> data = PageResponse.fromPage(dtoPage);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách bộ câu hỏi luyện tập thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/battle-sets")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getBattleSets(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "100") int limit
    ) {
        PageRequest pageRequest = PageRequest.of(page, limit, Sort.by(Sort.Direction.DESC, "taoLuc"));

        Page<BoCauHoi> result = boCauHoiService.findBattleSets(pageRequest);
        Page<BoCauHoiResponse> dtoPage = result.map(BoCauHoiResponse::from);
        PageResponse<BoCauHoiResponse> data = PageResponse.fromPage(dtoPage);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách bộ câu hỏi thi đấu thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/battle-sets/casual")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getCasualBattleSets() throws Exception {
        List<BoCauHoi> list = boCauHoiService.getBattleSetsCasualForCurrentUser();
        List<BoCauHoiResponse> data = list.stream()
                .map(BoCauHoiResponse::from)
                .toList();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Danh sách bộ câu hỏi cho trận CASUAL")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/battle-sets/ranked")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getRankedBattleSets() throws Exception {
        List<BoCauHoi> list = boCauHoiService.getBattleSetsRankedForCurrentUser();
        List<BoCauHoiResponse> data = list.stream()
                .map(BoCauHoiResponse::from)
                .toList();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Danh sách bộ câu hỏi cho trận RANKED")
                        .data(data)
                        .build()
        );
    }


    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getById(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        BoCauHoi boCauHoi = boCauHoiService.getById(id, currentUserId, isAdmin);
        BoCauHoiResponse boCauHoiResponse = BoCauHoiResponse.from(
                boCauHoi,
                boCauHoiService.hasUserUnlockedBo(boCauHoi.getId(), currentUserId)
        );
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy bộ câu hỏi thành công")
                        .status(HttpStatus.OK)
                        .data(boCauHoiResponse)
                        .build()
        );
    }

    @PostMapping("")
    @PreAuthorize("hasAnyRole('ROLE_USER', 'ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> create(@Valid @RequestBody BoCauHoiDTO boCauHoiDTO, BindingResult result) throws DataNotFoundException, PermissionDenyException {
        if (result.hasErrors()) {
            List<String> errorMessages = result.getFieldErrors()
                    .stream()
                    .map(FieldError::getDefaultMessage)
                    .toList();
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                    ResponseObject.builder()
                            .message(String.join(", ", errorMessages))
                            .status(HttpStatus.BAD_REQUEST)
                            .data(null)
                            .build()
            );

        }
        Long userId = securityUtils.getLoggedInUser().getId();
        BoCauHoi boCauHoi = boCauHoiService.create(boCauHoiDTO, userId);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Tao bo cau hoi thanh cong")
                .status(HttpStatus.OK)
                .data(boCauHoi)
                .build());
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_USER', 'ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> update(@PathVariable Long id, @RequestBody BoCauHoiDTO boCauHoiDTO) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUser().getId();
        boolean isAdmin = securityUtils.isAdmin();
        BoCauHoi boCauHoi = boCauHoiService.update(id, boCauHoiDTO, userId, isAdmin);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Cập nhật bộ câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(boCauHoi)
                .build());
    }

    @PutMapping("/{id}/approve")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> approve(@PathVariable Long id) {
        securityUtils.requireAdmin();
        Long adminId = securityUtils.getLoggedInUserId();
        BoCauHoi boCauHoi = boCauHoiService.approve(id, adminId);
        return ResponseEntity.ok(ResponseObject.builder()
                .message("Duyệt thành công")
                .status(HttpStatus.OK)
                .data(BoCauHoiResponse.from(boCauHoi))
                .build());
    }

    @PutMapping("/{id}/reject")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> reject(@PathVariable Long id, @RequestBody TuChoiBoCauHoiDTO lyDo) {
        securityUtils.requireAdmin();
        Long adminId = securityUtils.getLoggedInUser().getId();
//        return boCauHoiService.reject(id, lyDo, adminId);
        BoCauHoi boCauHoi = boCauHoiService.reject(id, lyDo, adminId);
        return ResponseEntity.ok(ResponseObject.builder()
                .message("Từ chối thành công")
                .status(HttpStatus.OK)
                .data(BoCauHoiResponse.from(boCauHoi))
                .build());
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> delete(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        boCauHoiService.softDelete(id, userId, isAdmin);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Xóa bộ câu hỏi thành công")
                        .status(HttpStatus.OK)
                        .data(null)
                        .build()
        );
    }

    @PutMapping("/{id}/mark-official")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> markOfficial(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long adminId = securityUtils.getLoggedInUserId(); // hoặc method tương đương của bạn
        BoCauHoi updated = boCauHoiService.markOfficial(id, adminId);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Đánh dấu bộ câu hỏi chính thức thành công")
                        .status(HttpStatus.OK)
                        .data(BoCauHoiResponse.from(updated))
                        .build()
        );
    }

    @PutMapping("/{id}/dis-mark-official")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> disMarkOfficial(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long adminId = securityUtils.getLoggedInUserId(); // hoặc method tương đương của bạn
        BoCauHoi updated = boCauHoiService.disMarkOfficial(id, adminId);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Hủy đánh dấu bộ câu hỏi chính thức thành công")
                        .status(HttpStatus.OK)
                        .data(BoCauHoiResponse.from(updated))
                        .build()
        );
    }


    @GetMapping("/{id}/thong-ke")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> thongKeBoCauHoi(@PathVariable Long id) throws DataNotFoundException {
        Map<String, Object> data = boCauHoiService.thongKeBoCauHoi(id);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Thống kê bộ câu hỏi thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @PutMapping("/unlock/{id}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> unlockBoCauHoi(@PathVariable("id") Long boCauHoiId) throws Exception {
        Long userId = securityUtils.getLoggedInUserId();

        UnlockBoCauHoiResponse data = boCauHoiService.unlockBoCauHoi(boCauHoiId, userId);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message(data.isDaMoKhoaTruocDo()
                                ? "Bộ câu hỏi đã được mở khóa từ trước"
                                : "Mở khóa bộ câu hỏi thành công")
                        .data(data)
                        .build()
        );
    }


}
