package com.app.backend.utils;

import org.apache.commons.io.FilenameUtils;
import org.springframework.core.io.UrlResource;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

public class FileUtils {
    private static final String UPLOADS_FOLDER = "uploads";

    // Các thư mục con riêng cho từng loại nội dung
    private static final String IMAGE_FOLDER = "images";
    private static final String AUDIO_FOLDER = "audios";
    private static final String VIDEO_FOLDER = "videos";

    /**
     * Xóa file theo tên (tìm trong thư mục uploads)
     */
    public static void deleteFile(String filename) throws IOException {
        Path uploadDir = Paths.get(UPLOADS_FOLDER);
        Path filePath = uploadDir.resolve(filename);
        if (Files.exists(filePath)) {
            Files.delete(filePath);
        }
    }

    /**
     * Kiểm tra xem có phải file ảnh hợp lệ không
     */
    public static boolean isImageFile(MultipartFile file) {
        String contentType = file.getContentType();
        return contentType.startsWith("image/");
    }

    /**
     * Kiểm tra xem có phải file âm thanh không
     */
    public static boolean isAudioFile(MultipartFile file) {
        String contentType = file.getContentType();
        return contentType.startsWith("audio/");
    }

    /**
     * Kiểm tra xem có phải file video không
     */
    public static boolean isVideoFile(MultipartFile file) {
        String contentType = file.getContentType();
        return contentType.startsWith("video/");
    }

    /**
     * Lưu file upload theo loại nội dung
     * @param file file upload
     * @param loai "HINH_ANH", "AM_THANH", "VIDEO"
     */
    public static String storeFile(MultipartFile file, String loai) throws IOException {
        if (file == null || file.isEmpty()) {
            throw new IOException("File rỗng hoặc không tồn tại");
        }

        String contentType = file.getContentType();
        if (contentType == null) {
            throw new IOException("Không thể xác định loại tệp");
        }

        // Xác định thư mục con phù hợp
        String subFolder;
        if ("HINH_ANH".equalsIgnoreCase(loai) && isImageFile(file)) {
            subFolder = IMAGE_FOLDER;
        } else if ("AM_THANH".equalsIgnoreCase(loai) && isAudioFile(file)) {
            subFolder = AUDIO_FOLDER;
        } else if ("VIDEO".equalsIgnoreCase(loai) && isVideoFile(file)) {
            subFolder = VIDEO_FOLDER;
        } else {
            throw new IOException("Định dạng tệp không hợp lệ cho loại: " + loai);
        }

        // Lấy tên file & extension
        String originalFilename = StringUtils.cleanPath(
                Objects.requireNonNull(file.getOriginalFilename())
        );
        String extension = FilenameUtils.getExtension(originalFilename);
        String uniqueFilename = UUID.randomUUID() + "_" + System.nanoTime() + "." + extension;

        // Tạo thư mục con nếu chưa tồn tại
        Path uploadPath = Paths.get(UPLOADS_FOLDER, subFolder);
        if (!Files.exists(uploadPath)) {
            Files.createDirectories(uploadPath);
        }

        // Lưu file vào thư mục
        Path destination = uploadPath.resolve(uniqueFilename);
        Files.copy(file.getInputStream(), destination, StandardCopyOption.REPLACE_EXISTING);

        // Trả về path tương đối cho FE
        return uniqueFilename;
    }

    public static UrlResource loadMediaResource(String fileName) {
        try {
            Path baseDir = Paths.get("uploads");

            // ✅ Kiểm tra từng thư mục con: images / videos / audios
            Path[] possiblePaths = new Path[]{
                    baseDir.resolve("images").resolve(fileName),
                    baseDir.resolve("videos").resolve(fileName),
                    baseDir.resolve("audios").resolve(fileName)
            };

            for (Path path : possiblePaths) {
                if (Files.exists(path)) {
                    return new UrlResource(path.toUri());
                }
            }

            // ✅ Nếu không tồn tại, trả ảnh mặc định
            Path defaultPath = baseDir.resolve("images/default-question.jpeg");
            if (Files.exists(defaultPath)) {
                return new UrlResource(defaultPath.toUri());
            }

        } catch (Exception ignored) {
        }
        return null;
    }

}
