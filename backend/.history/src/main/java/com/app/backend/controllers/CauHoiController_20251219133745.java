package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.CauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.CauHoi;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.cauhoi.CauHoiResponse;
import com.app.backend.services.cauhoi.ICauHoiService;
import com.app.backend.utils.FileUtils;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.UrlResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@RestController
@RequestMapping("${api.prefix}/cauHoi")
@RequiredArgsConstructor
public class CauHoiController {
    private final ICauHoiService cauHoiService;
    private final SecurityUtils securityUtils;

    @GetMapping("")
    public ResponseEntity<ResponseObject> getAll(
            @RequestParam(defaultValue = "0", name = "bo_cau_hoi_id") Long boCauHoiId,
            @RequestParam(defaultValue = "", name = "keyword") String keyword,
            @RequestParam(defaultValue = "", name = "do_kho") String doKho,
            @RequestParam(defaultValue = "", name = "loai_noi_dung") String loaiNoiDung,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) throws PermissionDenyException {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        PageRequest pageRequest = PageRequest.of(page, limit);
        var result = cauHoiService.findAll(
                boCauHoiId,
                keyword,
                doKho,
                loaiNoiDung,
                currentUserId,
                isAdmin,
                pageRequest
        );
        Page<CauHoiResponse> dtoPage = result.map(CauHoiResponse::from);

        // 4️⃣ Bọc trong PageResponse (generic)
        PageResponse<CauHoiResponse> data = PageResponse.fromPage(dtoPage);

        // 5️⃣ Trả về ResponseObject chuẩn REST
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách câu hỏi thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @PostMapping("/upload/{id}")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> uploadMedia(
            @PathVariable("id") Long cauHoiId,
            @RequestParam("file") MultipartFile file,
            @RequestParam(defaultValue = "HINH_ANH") String loaiNoiDung
    ) {
        if (file == null || file.isEmpty()) {
            return ResponseEntity.badRequest().body(
                    ResponseObject.builder()
                            .message("Image file is required.")
                            .build()
            );
        }

        if (file.getSize() > 10 * 1024 * 1024) { // 10MB
            return ResponseEntity.status(HttpStatus.PAYLOAD_TOO_LARGE)
                    .body(ResponseObject.builder()
                            .message("Image file size exceeds the allowed limit of 10MB.")
                            .status(HttpStatus.PAYLOAD_TOO_LARGE)
                            .build());
        }
        try {
            String fileUrl = FileUtils.storeFile(file, loaiNoiDung);
            cauHoiService.changeMedia(cauHoiId, fileUrl);
            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .status(HttpStatus.OK)
                            .message("Upload " + loaiNoiDung + " thành công")
                            .data(fileUrl)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(
                    ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message("Upload thất bại: " + e.getMessage())
                            .data(null)
                            .build()
            );
        }
    }

    @GetMapping("/media/{fileName}")
    public ResponseEntity<?> viewMedia(@PathVariable String fileName) {
        try {
            UrlResource resource = FileUtils.loadMediaResource(fileName);
            if (resource == null || !resource.exists()) {
                return ResponseEntity.notFound().build();
            }

            // Xác định MIME
            String mimeType = Files.probeContentType(Paths.get(resource.getURI()));
            MediaType mediaType = (mimeType != null)
                    ? MediaType.parseMediaType(mimeType)
                    : MediaType.APPLICATION_OCTET_STREAM;

            // ✅ Trả trực tiếp resource (ảnh, video, audio)
            return ResponseEntity.ok()
                    .contentType(mediaType)
                    .body(resource);

        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }


    @GetMapping("/{id}")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getById(@PathVariable("id") Long id) throws DataNotFoundException, PermissionDenyException {
        Long uid = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        CauHoi cauhoi = cauHoiService.findById(id, uid, isAdmin);
        CauHoiResponse cauHoiResponse = CauHoiResponse.from(cauhoi);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Lấy câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(cauHoiResponse)
                .build());
    }

    @GetMapping("/bo/{boCauHoiId}")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getByBoCauHoi(
            @PathVariable Long boCauHoiId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) throws PermissionDenyException {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        PageRequest pageable = PageRequest.of(page, limit);
        
        // ✅ Sử dụng findAll với boCauHoiId để có logic check unlock
        Page<CauHoi> cauHoiPage = cauHoiService.findAll(
                boCauHoiId,
                "", // keyword
                "", // doKho
                "", // loaiNoiDung
                currentUserId,
                isAdmin,
                pageable
        );

        Page<CauHoiResponse> dtoPage = cauHoiPage.map(CauHoiResponse::from);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách câu hỏi theo bộ thành công")
                        .status(HttpStatus.OK)
                        .data(PageResponse.fromPage(dtoPage))
                        .build()
        );
    }

    @PostMapping("")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> create(@Valid @RequestBody CauHoiDTO cauHoiDTO,
                                                 BindingResult result) throws DataNotFoundException, PermissionDenyException {
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
        Long uid = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        CauHoi cauHoi = cauHoiService.create(cauHoiDTO, uid, isAdmin);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Tạo câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(cauHoi)
                .build());
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> update(@PathVariable Long id, @RequestBody CauHoiDTO dto) throws DataNotFoundException, PermissionDenyException {
        Long uid = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        CauHoi cauHoi = cauHoiService.update(id, dto, uid, isAdmin);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Cập nhật câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(cauHoi)
                .build());
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> delete(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long uid = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        cauHoiService.delete(id, uid, isAdmin);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Xoá câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(null)
                .build());
    }
}
