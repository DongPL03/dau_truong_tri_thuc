package com.app.backend.controllers;

import com.app.backend.components.LocalizationUtils;
import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.*;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.InvalidPasswordException;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.Token;
import com.app.backend.models.constant.TrangThaiNguoiDung;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.user.LoginResponse;
import com.app.backend.responses.user.UserListResponse;
import com.app.backend.responses.user.UserResponse;
import com.app.backend.responses.user.UserSummaryResponse;
import com.app.backend.services.bangxephang.IBangXepHangService;
import com.app.backend.services.nguoidung.INguoiDungService;
import com.app.backend.services.token.ITokenService;
import com.app.backend.services.verifyemail.IEmailVerificationService;
import com.app.backend.utils.FileUtils;
import com.app.backend.utils.MessageKeys;
import com.app.backend.utils.ValidationUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.UrlResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

@RestController
@RequestMapping("${api.prefix}/users")
@RequiredArgsConstructor
public class NguoiDungController {
    private final INguoiDungService userService;
    private final LocalizationUtils localizationUtils;
    private final UserDetailsService userDetailsService;
    private final ITokenService tokenService;
    private final SecurityUtils securityUtils;
    private final IEmailVerificationService emailVerificationService;
    private final IBangXepHangService bangXepHangService;
    private final com.app.backend.repositories.INguoiDungRepository userRepository;

    @GetMapping("")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getAllUser(
            @RequestParam(defaultValue = "", required = false) String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) throws Exception {
        // Tạo Pageable từ thông tin trang và giới hạn
        PageRequest pageRequest = PageRequest.of(
                page, limit,
                //Sort.by("createdAt").descending()
                Sort.by("id").ascending()
        );
        Page<UserResponse> userPage = userService.findAll(keyword, pageRequest)
                .map(UserResponse::fromUser);

        // Lấy tổng số trang
        int totalPages = userPage.getTotalPages();
        List<UserResponse> userResponses = userPage.getContent();
        UserListResponse userListResponse = UserListResponse
                .builder()
                .users(userResponses)
                .totalPages(totalPages)
                .build();
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Get user list successfully")
                .status(HttpStatus.OK)
                .data(userListResponse)
                .build());
    }

    @PostMapping("/register")
    //can we register an "admin" user ?
    public ResponseEntity<ResponseObject> createUser(
            @Valid @RequestBody UserDTO userDTO,
            BindingResult result
    ) throws Exception {
        if (result.hasErrors()) {
            List<String> errorMessages = result.getFieldErrors()
                    .stream()
                    .map(FieldError::getDefaultMessage)
                    .toList();

            // Trả về chuỗi thông báo, ví dụ lấy phần tử đầu tiên
            String finalMessage = errorMessages.isEmpty() ? "Validation failed" : errorMessages.get(0);

            return ResponseEntity.badRequest().body(ResponseObject.builder()
                    .status(HttpStatus.BAD_REQUEST)
                    .data(null)
                    .message(finalMessage) // Lấy thông báo lỗi đầu tiên
                    .build());
        }
        if (userDTO.getEmail() == null || userDTO.getEmail().trim().isBlank()) {
            if (userDTO.getTenDangNhap() == null || userDTO.getTenDangNhap().isBlank()) {
                return ResponseEntity.badRequest().body(ResponseObject.builder()
                        .status(HttpStatus.BAD_REQUEST)
                        .data(null)
                        .message("At least email or phone number is required")
                        .build());
            } else {
                //phone number not blank
                if (!ValidationUtils.isValidPhoneNumber(userDTO.getTenDangNhap())) {
                    throw new Exception("Invalid");
                }
            }
        } else {
            //Email not blank
            if (!ValidationUtils.isValidEmail(userDTO.getEmail())) {
                throw new Exception("Invalid email format");
            }
        }

        if (!userDTO.getPassword().equals(userDTO.getRetypePassword())) {
            //registerResponse.setMessage();
            return ResponseEntity.badRequest().body(ResponseObject.builder()
                    .status(HttpStatus.BAD_REQUEST)
                    .data(null)
                    .message(localizationUtils.getLocalizedMessage(MessageKeys.PASSWORD_NOT_MATCH))
                    .build());
        }
        NguoiDung user = userService.createUser(userDTO);

        //verify email if provided
//        if (user.getEmail() != null && !user.getEmail().isBlank()) {
//            emailVerificationService.createAndSendTokenForUser(user.getId());
//        }
        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.CREATED)
                .data(UserResponse.fromUser(user))
                .message("Account registration successful")
                .build());
    }

    @GetMapping("idVaiTro/{tenDangNhap}")
    public Long getRoleNameByUsername(
            @PathVariable String tenDangNhap) {
        Long roleId = userService.findIdVaiTroByTenDangNhap(tenDangNhap);
        return roleId;
    }

    @PostMapping("/login")
    public ResponseEntity<ResponseObject> login(
            @Valid @RequestBody UserLoginDTO userLoginDTO,
            HttpServletRequest request
    ) throws Exception {
        // Gọi hàm login từ UserService cho đăng nhập truyền thống
        String token = userService.login(userLoginDTO);
        NguoiDung userDetails = (NguoiDung) userDetailsService.loadUserByUsername(userLoginDTO.getTenDangNhap());

        // Xử lý token và thông tin người dùng
        String userAgent = request.getHeader("User-Agent");
        NguoiDung userDetail = userService.getUserDetailsFromToken(token);
        Token jwtToken = tokenService.addToken(userDetail, token, isMobileDevice(userAgent));

        // Tạo đối tượng LoginResponse
        LoginResponse loginResponse = LoginResponse.builder()
                .message(localizationUtils.getLocalizedMessage(MessageKeys.LOGIN_SUCCESSFULLY))
                .token(jwtToken.getToken())
                .tokenType(jwtToken.getTokenType())
                .refreshToken(jwtToken.getRefreshToken())
                .username(userDetail.getUsername())
                .roles(userDetail.getAuthorities().stream().map(GrantedAuthority::getAuthority).toList())
                .id(userDetail.getId())
                .status(userDetail.getTrangThai())
                .build();

        // Trả về phản hồi
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Login successfully")
                        .data(loginResponse)
                        .status(HttpStatus.OK)
                        .build()
        );
    }

    private boolean isMobileDevice(String userAgent) {
        // Kiểm tra User-Agent header để xác định thiết bị di động
        // Ví dụ đơn giản:
        return userAgent.toLowerCase().contains("mobile");
    }

    @GetMapping("/verify-email")
    public ResponseEntity<ResponseObject> verifyEmail(@RequestParam("token") String token) {
        try {
            emailVerificationService.verifyToken(token);
            return ResponseEntity.ok(ResponseObject.builder()
                    .message("Email verified successfully. You can login now.")
                    .status(HttpStatus.OK).build());
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(ResponseObject.builder()
                    .message("Invalid verification token")
                    .status(HttpStatus.BAD_REQUEST).build());
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(ResponseObject.builder()
                    .message(e.getMessage())
                    .status(HttpStatus.BAD_REQUEST).build());
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(ResponseObject.builder()
                    .message("Internal error")
                    .status(HttpStatus.INTERNAL_SERVER_ERROR).build());
        }
    }

    @PostMapping("/resend-verification")
    public ResponseEntity<ResponseObject> resendVerification(@RequestParam("email") String email) {
        try {
            emailVerificationService.resendForEmail(email);
            return ResponseEntity.ok(ResponseObject.builder()
                    .message("Verification email resent successfully")
                    .status(HttpStatus.OK).build());
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(ResponseObject.builder()
                    .message("Email not found")
                    .status(HttpStatus.BAD_REQUEST).build());
        } catch (IllegalStateException e) {
            return ResponseEntity.badRequest().body(ResponseObject.builder()
                    .message(e.getMessage())
                    .status(HttpStatus.BAD_REQUEST).build());
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(ResponseObject.builder()
                    .message("Internal error")
                    .status(HttpStatus.INTERNAL_SERVER_ERROR).build());
        }
    }

    @PostMapping("/details")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getUserDetails(
            @RequestHeader("Authorization") String authorizationHeader
    ) throws Exception {
        String extractedToken = authorizationHeader.substring(7); // Loại bỏ "Bearer " từ chuỗi token
        NguoiDung user = userService.getUserDetailsFromToken(extractedToken);
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Get user's detail successfully")
                        .data(UserResponse.fromUser(user))
                        .status(HttpStatus.OK)
                        .build()
        );
    }

    @PutMapping("/details/{userId}")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
//    @Operation(security = { @SecurityRequirement(name = "bearer-key") })
    public ResponseEntity<ResponseObject> updateUserDetails(
            @PathVariable Long userId,
            @RequestBody UpdateUserDTO updatedUserDTO,
            @RequestHeader("Authorization") String authorizationHeader
    ) throws Exception {
        String extractedToken = authorizationHeader.substring(7);
        NguoiDung user = userService.getUserDetailsFromToken(extractedToken);
        // Ensure that the user making the request matches the user being updated
        if (!Objects.equals(user.getId(), userId)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
        }
        NguoiDung updatedUser = userService.updateUser(userId, updatedUserDTO);
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Update user detail successfully")
                        .data(UserResponse.fromUser(updatedUser))
                        .status(HttpStatus.OK)
                        .build()
        );
    }

    @PutMapping("/reset-password/{userId}")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> resetPassword(@Valid @PathVariable long userId) {
        try {
            String newPassword = UUID.randomUUID().toString().substring(0, 5); // Tạo mật khẩu mới
            userService.resetPassword(userId, newPassword);
            return ResponseEntity.ok(ResponseObject.builder()
                    .message("Reset password successfully")
                    .data(newPassword)
                    .status(HttpStatus.OK)
                    .build());
        } catch (InvalidPasswordException e) {
            return ResponseEntity.ok(ResponseObject.builder()
                    .message("Invalid password")
                    .data("")
                    .status(HttpStatus.BAD_REQUEST)
                    .build());
        } catch (DataNotFoundException e) {
            return ResponseEntity.ok(ResponseObject.builder()
                    .message("User not found")
                    .data("")
                    .status(HttpStatus.BAD_REQUEST)
                    .build());
        }
    }

    @PutMapping("/block/{userId}/{active}")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> blockOrEnable(
            @Valid @PathVariable long userId,
            @Valid @PathVariable int active
    ) throws Exception {
        userService.blockOrEnable(userId, active > 0);
        String message = active > 0 ? "Successfully enabled the user." : "Successfully blocked the user.";
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message(message)
                .status(HttpStatus.OK)
                .data(null)
                .build());
    }

    @GetMapping("details/{userId}")
    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getUserDetailsById(
            @PathVariable Long userId
    ) throws Exception {
        NguoiDung user = userService.getUserById(userId);
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Get user's detail successfully")
                        .data(UserResponse.fromUser(user))
                        .status(HttpStatus.OK)
                        .build()
        );
    }

    @PostMapping(value = "/upload-profile-image", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> uploadProfileImage(
            @RequestParam("file") MultipartFile file
    ) throws Exception {
        NguoiDung loginUser = securityUtils.getLoggedInUser();
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

        // Check file type
        if (!FileUtils.isImageFile(file)) {
            return ResponseEntity.status(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
                    .body(ResponseObject.builder()
                            .message("Uploaded file must be an image.")
                            .status(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
                            .build());
        }

        // Store file and get filename
        String oldFileName = loginUser.getAvatarUrl();
        String imageName = FileUtils.storeFile(file, "HINH_ANH");

        userService.changeProfileImage(loginUser.getId(), imageName);
        // Delete old file if exists
        if (!StringUtils.isEmpty(oldFileName)) {
            FileUtils.deleteFile(oldFileName);
        }
//1aba82e1-4599-4c8b-8ec5-9c16e5aad379_3734888057500.png
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Upload profile image successfully")
                .status(HttpStatus.CREATED)
                .data(imageName) // Return the filename or image URL
                .build());
    }

    @GetMapping("/profile-images/{imageName}")
    public ResponseEntity<?> viewImage(@PathVariable String imageName) {
        try {
            java.nio.file.Path imagePath = Paths.get("uploads/images/" + imageName);
            UrlResource resource = new UrlResource(imagePath.toUri());

            if (resource.exists()) {
                return ResponseEntity.ok()
                        .contentType(MediaType.IMAGE_JPEG)
                        .body(resource);
            } else {
                return ResponseEntity.ok()
                        .contentType(MediaType.IMAGE_JPEG)
                        .body(new UrlResource(Paths.get("uploads/images/default-profile-image.jpeg").toUri()));
                //return ResponseEntity.notFound().build();
            }
        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }


    @PostMapping("/refreshToken")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> refreshToken(@Valid @RequestBody RefreshTokenRequestDTO req) throws
            Exception {
        // Lấy user từ refresh token, kiểm tra revoke/expire trong DB
        NguoiDung user = userService.getUserDetailsFromRefreshToken(req.refreshToken());
        // Cấp token mới (access + refresh rotation) và revoke cái cũ
        Token newToken = tokenService.refreshToken(req.refreshToken(), user);

        LoginResponse payload = LoginResponse.builder()
                .message("REFRESH_OK")
                .token(newToken.getToken())
                .tokenType(newToken.getTokenType())
                .refreshToken(newToken.getRefreshToken())
                .id(user.getId())
                .username(user.getUsername())
                .roles(user.getAuthorities().stream().map(a -> a.getAuthority()).toList())
                .build();
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Token refreshed successfully")
                        .data(payload)
                        .status(HttpStatus.OK)
                        .build());
    }

    @GetMapping("/{id}/summary")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getUserSummary(@PathVariable("id") Long userId) throws DataNotFoundException {
        UserSummaryResponse data = bangXepHangService.getUserSummary(userId);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thông tin tổng quan người dùng thành công")
                        .data(data)
                        .build()
        );
    }


    // 2) USER TỰ ĐỔI MẬT KHẨU (check oldPassword)
    @PutMapping("/change-password")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> changePassword(@Valid @RequestBody ChangePasswordDTO body) throws
            Exception {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        NguoiDung current = (NguoiDung) auth.getPrincipal();

        userService.changePasswordWithOldCheck(current.getId(), body.oldPassword(), body.newPassword());
        // revoke toàn bộ token đang hoạt động để buộc login lại
        tokenService.revokeAllTokensForUser(current);
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Change password successfully")
                        .data(null)
                        .status(HttpStatus.OK)
                        .build());
    }

    // 3) ADMIN UPDATE ROLE
    @PutMapping("/role/{userId}")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> updateRole(
            @PathVariable Long userId,
            @Valid @RequestBody UpdateRoleRequestDTO request
    ) throws Exception {
        // Trong SecurityConfig bạn đã hạn chế /users/role/** = ADMIN, nên ở đây không cần check lại
        userService.updateRole(userId, request.role());
        return ResponseEntity.ok().body(
                ResponseObject.builder()
                        .message("Update role successfully")
                        .data(null)
                        .status(HttpStatus.OK)
                        .build());
    }

    // --- BỔ SUNG SELF-ONLY GUARD cho cập nhật hồ sơ ---
    // Ví dụ bạn đã có: PUT /users/details/{userId}
//    @PutMapping("/details/{userId}")
//    public ResponseEntity<ResponseObject> updateDetails(
//            @PathVariable Long userId,
//            @RequestBody  Object updateRequest
//    ) throws Exception {
//        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
//        NguoiDung current = (NguoiDung) auth.getPrincipal();
//
//        boolean isAdmin = current.getAuthorities()
//                .stream().anyMatch(a -> a.getAuthority().equals("ROLE_ADMIN"));
//
//        if (!isAdmin && !current.getId().equals(userId)) {
//            return ResponseEntity.status(HttpStatus.FORBIDDEN)
//                    .body(ResponseObject.fail("You can only update your own profile"));
//        }
//
//        // Gọi service sẵn có của bạn
//        var result = userService.updateUser(userId, updateRequest);
//        return ResponseEntity.ok(ResponseObject.ok(result));
//    }

    @PutMapping("/deactivate")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> deactivateAccount() {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            NguoiDung currentUser = (NguoiDung) auth.getPrincipal();
            userService.softDeleteUser(currentUser.getId());
            tokenService.revokeAllTokensForUser(currentUser); // buộc logout

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Your account has been deactivated successfully.")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(ResponseObject.builder()
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .message("Failed to deactivate account: " + e.getMessage())
                    .build());
        }
    }

    @PutMapping("/restore/{userId}")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> restoreUser(@PathVariable Long userId) {
        try {
            userService.restoreUser(userId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("User restored successfully.")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(ResponseObject.builder()
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .message("Failed to restore user: " + e.getMessage())
                    .build());
        }
    }


    @PostMapping("/logout")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> logout(HttpServletRequest request) {
        try {
            // Lấy token từ header Authorization
            String header = request.getHeader("Authorization");
            Long userId = securityUtils.getLoggedInUserIdSafe();
            if (userId != null) {
                userRepository.findById(userId).ifPresent(u -> {
                    u.setTrangThai(TrangThaiNguoiDung.OFFLINE);
                    userRepository.save(u);
                });
            }
            if (header == null || !header.startsWith("Bearer ")) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(
                        ResponseObject.builder()
                                .status(HttpStatus.UNAUTHORIZED)
                                .message("Missing or invalid Authorization header")
                                .build()
                );
            }

            String token = header.substring(7); // bỏ "Bearer "
            tokenService.revokeToken(token);


            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Logout successfully")
                    .build());

        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(
                    ResponseObject.builder()
                            .status(HttpStatus.INTERNAL_SERVER_ERROR)
                            .message("Logout failed: " + e.getMessage())
                            .build()
            );
        }
    }
}
